{-# LANGUAGE FlexibleContexts #-}
module Web.Google.Maps ( mkEnv
                       , mkEnvWithMan
                       , runGoogleMaps
                       , queryGeocode
                       , queryDistMatrix
                       , module X
                       ) where

import Control.Monad.IO.Class    (liftIO, MonadIO)
import Control.Monad.Reader      (runReaderT, MonadReader)
import Network.HTTP.Client       (Manager, newManager)
import Network.HTTP.Client.TLS   (tlsManagerSettings)

import Web.Google.Maps.Types as X
import Web.Google.Maps.Internal
import qualified Web.Google.Maps.Services.Geocode as Geo
import qualified Web.Google.Maps.Services.DistanceMatrix as Dist

mkEnvWithMan :: APIKey -> Manager-> Env
mkEnvWithMan = Env

mkEnv :: MonadIO m => APIKey -> m Env
mkEnv key = do
  man <- liftIO $ newManager tlsManagerSettings
  return $ Env key man

runGoogleMaps :: MonadIO m => Env -> GoogleMapsT m a -> m a
runGoogleMaps env action = runReaderT (runGoogleMapsT action) env

queryGeocode :: (MonadIO m, MonadReader Env m)
             => Geo.GeocodeRequest
             -> m Geo.GeocodeResponse
queryGeocode = http Geo.webService

queryDistMatrix :: (MonadIO m, MonadReader Env m)
                => Dist.DMRequest
                -> m Dist.DMResponse
queryDistMatrix = http Dist.webService
