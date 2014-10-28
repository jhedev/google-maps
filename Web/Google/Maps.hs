module Web.Google.Maps ( defaultConfig
                       , defaultConfigWithMan
                       , runGoogleMaps
                       --, getCoordinates
                       -- , getDistanceByCar
                       , module X
                       ) where

import Web.Google.Maps.Types as X
import Web.Google.Maps.Geocode as X
import Web.Google.Maps.DistanceMatrix as X

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Text (Text)
import Network.HTTP.Conduit

defaultConfigWithMan :: MonadIO m => APIKey -> Manager-> m GoogleMapsConfig
defaultConfigWithMan key man = do
  return $ GoogleMapsConfig key man

defaultConfig :: MonadIO m => APIKey -> m GoogleMapsConfig
defaultConfig key = do
  man <- liftIO $ newManager conduitManagerSettings
  return $ GoogleMapsConfig key man

runGoogleMaps :: (MonadIO m) => GoogleMapsConfig -> GoogleMaps a -> m a
runGoogleMaps config action = liftIO $ runResourceT $ runReaderT action config
