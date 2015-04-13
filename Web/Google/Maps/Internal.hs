{-# LANGUAGE FlexibleContexts #-}
module Web.Google.Maps.Internal
       ( http
       ) where

import Web.Google.Maps.Types

import Control.Monad.Reader
import Data.Aeson (decode, FromJSON)
import Data.Maybe (fromJust)
import Network.HTTP.Conduit (parseUrl, responseBody, httpLbs)
import Network.URL

baseURL :: String -> URL
baseURL service = fromJust $ importURL $
                      concat [ "http://maps.googleapis.com/maps/api/"
                             , service
                             , "/json"]

endpointURL :: APIKey
            -> WebService a b
            -> a
            -> URL
endpointURL key webservice request =
    foldl add_param (baseURL service) $ ("key", key) : params request
  where
    service = getServiceName webservice
    params  = getParams webservice

http :: ( MonadIO m
        , MonadReader Env m
        , FromJSON res)
     => WebService req res
     -> req
     -> m res
http webservice request = do
    config <- ask
    let key = apiKey config
    initReq <- liftIO $ parseUrl $ exportURL $ endpointURL key webservice request
    res <- httpLbs initReq $ manager config
    case decode $ responseBody res of
      Just result -> return result
      _ -> error "Could not decode"
