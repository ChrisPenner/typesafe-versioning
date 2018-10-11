{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Before where

import Control.Monad.IO.Class
import Data.Aeson

-- User -----------------------------------
data User = User
  { name :: String
  } deriving (Show)

instance ToJSON (User) where
  toJSON (User {name}) = object ["name" .= name]

-- App Monad --------------------------------
newtype AppM a = AppM
  { runApp :: IO a
  } deriving (Functor, Applicative, Monad, MonadIO)

-- User Service --------------------------------
class (Monad m) =>
      MonadUserService m
  where
  getUser :: m User

instance MonadUserService AppM where
  getUser = return (User "Bob Johnson")

-- App -----------------------------------------
userHandler :: (MonadUserService m) => m Value
userHandler = do
  user <- getUser
  return $ toJSON user

app :: (MonadIO m, MonadUserService m) => m ()
app = do
  userJSON <- userHandler
  liftIO $ print userJSON

main :: IO ()
main = runApp app
