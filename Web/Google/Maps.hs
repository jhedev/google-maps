module Web.Google.Maps ( mkEnv
                       , mkEnvWithMan
                       , runGoogleMapsT
                       , module X
                       ) where

import Web.Google.Maps.Types as X
import Web.Google.Maps.Geocode as X
import Web.Google.Maps.DistanceMatrix as X

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (runReaderT)
import Network.HTTP.Conduit (Manager, newManager, conduitManagerSettings)

mkEnvWithMan :: APIKey -> Manager-> Env
mkEnvWithMan = Env

mkEnv :: MonadIO m => APIKey -> m Env
mkEnv key = do
  man <- liftIO $ newManager conduitManagerSettings
  return $ Env key man

runGoogleMapsT :: GoogleMapsT m a -> Env -> m a
runGoogleMapsT (GoogleMapsT k) = runReaderT k
