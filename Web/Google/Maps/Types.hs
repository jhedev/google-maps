module Web.Google.Maps.Types where

import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Trans.Reader (ReaderT)
import Network.HTTP.Conduit (Manager)

type APIKey = String

data GoogleMapsConfig = GoogleMapsConfig
    { googleMapsApiKey :: APIKey
    , googleMapsManager :: Manager
    }

type GoogleMaps a = ReaderT GoogleMapsConfig (ResourceT IO) a


data GoogleMapsWebService req res = GoogleMapsWebService
  { getServiceName :: String
  , getParams :: req -> [(String, String)]
  }
