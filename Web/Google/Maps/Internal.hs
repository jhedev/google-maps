{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Google.Maps.Internal
       ( http
       ) where

import Web.Google.Maps.Types

import Control.Monad.Reader
import Data.Aeson (decode, FromJSON)
import Network.HTTP.Client (Request, parseUrl, responseBody, httpLbs,
                            setQueryString)

baseUrl :: String -> String
baseUrl service = concat [ "https://maps.googleapis.com/maps/api/"
                         , service
                         , "/json"]

endpointReq :: APIKey
            -> WebService a b
            -> a
            -> IO Request
endpointReq key webservice request = do
    initReq <- parseUrl $ baseUrl service
    return $ setQueryString params initReq
  where
    service = getServiceName webservice
    params  = ("key", Just key) : getParams webservice request

http :: ( MonadIO m
        , MonadReader Env m
        , FromJSON res)
     => WebService req res
     -> req
     -> m res
http webservice request = do
    config <- ask
    let key = apiKey config
    initReq <- liftIO $ endpointReq key webservice request
    res <- liftIO $ httpLbs initReq $ manager config
    case decode $ responseBody res of
      Just result -> return result
      _ -> error "Could not decode"
