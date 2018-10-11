{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Before2 where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Kind
import GHC.TypeLits

-- User -----------------------------------
data User (v :: Nat) where
  UserV1 :: { name :: String} -> User 1
  UserV2
    :: { firstName :: String
       , lastName :: String}
    -> User 2

instance ToJSON (User 1) where
  toJSON (UserV1 {name}) = object ["name" .= name]

instance ToJSON (User 2) where
  toJSON (UserV2 {firstName, lastName}) =
    object ["firstName" .= firstName, "lastName" .= lastName]

-- App Monad --------------------------------
newtype AppM (userVersion :: Nat) (postVersion :: Nat) a = AppM
  { runApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- User Service --------------------------------
class (Monad m) =>
      MonadUserService v m
  | m -> v
  where
  getUser :: m (User v)

instance MonadUserService 1 (AppM 1 n) where
  getUser = return (UserV1 "Bob Johnson")

instance MonadUserService 2 (AppM 2 n) where
  getUser = return (UserV2 "Bob" "Johnson")

-- App -----------------------------------------
userHandler :: (ToJSON (User v), MonadUserService v m) => m Value
userHandler = do
  user <- getUser
  return $ toJSON user

app :: (ToJSON (User v), MonadIO m, MonadUserService v m) => m ()
app = do
  userJSON <- userHandler
  liftIO $ print userJSON

main :: IO ()
main = runAppWithCheck (app :: AppM 2 1 ())

runAppWithCheck ::
     Compatible userVersion postVersion
  => AppM userVersion postVersion a
  -> IO a
-- We only really need the additional type information, under the hood we can just call `runApp`
runAppWithCheck = runApp

type family Compatible (userVersion :: Nat) (postVersion :: Nat) :: Constraint where
  Compatible 1 1 = ()
  Compatible 2 3 = ()
  Compatible 2 4 = ()
  Compatible a b = TypeError (Text "userVersion " :<>: ShowType a :<>: Text " is not compatible with postVersion " :<>: ShowType b)
