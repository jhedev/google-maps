{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Google.Maps.Types ( APIKey
                             , Env (..)
                             , GoogleMaps
                             , GoogleMapsT (..)
                             , WebService (..)
                             ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Reader.Class (MonadReader)
import Network.HTTP.Client (Manager)

type APIKey = String

data Env = Env
    { apiKey :: APIKey
    , manager :: Manager
    }

type GoogleMaps a = GoogleMapsT IO a

newtype GoogleMapsT m a = GoogleMapsT
  { unGoogleMaps :: ReaderT Env m a}
  deriving (Functor, Applicative, Monad, MonadReader Env)

data WebService req res = WebService
  { getServiceName :: String
  , getParams :: req -> [(String, String)]
  }
