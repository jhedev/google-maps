module Web.Google.Maps.Internal
       ( queryAPI
       ) where

import Web.Google.Maps.Types

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Aeson (decode, FromJSON)
import Data.Maybe (fromJust)
import Network.HTTP.Conduit (parseUrl, responseBody, httpLbs)
import Network.URL

baseURL :: String -> URL
baseURL service = fromJust $ importURL $
                      concat [ "http://maps.googleapis.com/maps/api/"
                             , service
                             , "/json"]

apiEndpointURL :: APIKey -> GoogleMapsWebService a b -> a -> URL
apiEndpointURL key webservice request =
    foldl add_param (baseURL service) $  params request
  where
    service =  getServiceName webservice
    params  = getParams webservice

queryAPI :: FromJSON res => GoogleMapsWebService req res -> req -> GoogleMaps res
queryAPI webservice request = do
    config <- ask
    let key = googleMapsApiKey config
    initReq <- liftIO $ parseUrl $ exportURL $ apiEndpointURL key webservice request
    res <- httpLbs initReq $ googleMapsManager config
    case decode $ responseBody res of
      Just result -> return result
      _ -> error "Could not decode"
