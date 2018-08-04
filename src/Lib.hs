{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Char
import Data.Kind
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import GHC.Generics
import GHC.TypeLits

-- | Version-Annotating Data Kinds
data AppVersion =
  AppVersion Nat

data UserServiceVersion =
  UserServiceVersion Nat

-- User -----------------------------------
data User (v :: Nat) where
  UserV1 :: { name :: String} -> User 1
  UserV2
    :: { firstName :: String
       , lastName :: String}
    -> User 2

deriving instance Show (User v)

instance ToJSON (User 1) where
  toJSON (UserV1 {..}) = object ["name" .= name]

instance ToJSON (User 2) where
  toJSON (UserV2 {..}) =
    object ["firstName" .= firstName, "lastName" .= lastName]

-- App Monad --------------------------------
newtype AppM appVersion serviceVersion a = AppM
  { runApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- User Service --------------------------------
class (Monad m, KnownNat v) =>
      MonadUserService v m
  | m -> v
  where
  getUser :: m (User v)

instance MonadUserService 1 (AppM av ('UserServiceVersion 1)) where
  getUser = return (UserV1 "bob johnson")

instance MonadUserService 2 (AppM av ('UserServiceVersion 2)) where
  getUser = return (UserV2 "bob" "johnson")

-- App -----------------------------------------
userHandler :: (ToJSON (User v), MonadUserService v m) => m Value
userHandler = do
  user <- getUser
  return $ toJSON user

app :: (MonadIO m, MonadUserService v m, ToJSON (User v)) => m ()
app = do
  printVersion
  userJSON <- userHandler
  liftIO $ print userJSON

printVersion ::
     forall v m. (MonadIO m, MonadUserService v m)
  => m ()
printVersion =
  liftIO . putStrLn $ "UserService version: " ++ show (natVal (Proxy @v))

runApp' :: Compatible av sv => AppM av sv a -> IO a
runApp' = runApp

mainApp :: IO ()
mainApp = runApp' @('AppVersion 1) @('UserServiceVersion 2) app

type family Compatible a b :: Constraint where
  Compatible ('AppVersion 1) ('UserServiceVersion 1) = ()
  Compatible ('AppVersion 1) ('UserServiceVersion 2) = ()
  Compatible ('AppVersion 2) ('UserServiceVersion 3) = ()
  Compatible a b = TypeError (ShowType a :<>: Text " is not compatible with " :<>: ShowType b)
