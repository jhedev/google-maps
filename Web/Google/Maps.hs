{-# LANGUAGE FlexibleContexts #-}
module Web.Google.Maps ( mkEnv
                       , mkEnvWithMan
                       , runGoogleMapsT
                       , queryGeocode
                       , queryDistMatrix
                       , module X
                       ) where

import Web.Google.Maps.Types as X
import Web.Google.Maps.Internal
import qualified Web.Google.Maps.Services.Geocode as Geo
import qualified Web.Google.Maps.Services.DistanceMatrix as Dist

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (runReaderT, MonadReader)
import Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)

mkEnvWithMan :: APIKey -> Manager-> Env
mkEnvWithMan = Env

mkEnv :: MonadIO m => APIKey -> m Env
mkEnv key = do
  man <- liftIO $ newManager conduitManagerSettings
  return $ Env key man

runGoogleMapsT :: GoogleMapsT m a -> Env -> m a
runGoogleMapsT (GoogleMapsT k) = runReaderT k

queryGeocode :: (MonadIO m, MonadReader Env m) => Geo.GeocodeRequest -> m Geo.GeocodeResponse
queryGeocode = http Geo.webService

queryDistMatrix :: (MonadIO m, MonadReader Env m) => Dist.DMRequest -> m Dist.DMResponse
queryDistMatrix = http Dist.webService
