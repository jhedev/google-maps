module Web.Google.Maps ( defaultGoogleMapsConfig
                       , runGoogleMaps
                       --, getCoordinates
                       , getDistanceByCar
                       , module Web.Google.Maps.Types
                       , module Web.Google.Maps.Geocode
                       , module Web.Google.Maps.DistanceMatrix
                       --, test
                       ) where

import Web.Google.Maps.Types
import Web.Google.Maps.Geocode
import Web.Google.Maps.DistanceMatrix

import Control.Monad.Trans.Resource
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Text (Text)
import Network.HTTP.Conduit


defaultConfig :: MonadIO m => APIKey -> m GoogleMapsConfig
defaultConfig key = do
  man <- liftIO $ newManager conduitManagerSettings
  return $ GoogleMapsConfig key man

runGoogleMaps :: (MonadIO m) => GoogleMapsConfig -> GoogleMaps a -> m a
runGoogleMaps config action = liftIO $ runResourceT $ runReaderT action config
