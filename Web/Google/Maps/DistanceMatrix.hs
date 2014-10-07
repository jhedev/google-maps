{-# LANGUAGE RecordWildCards #-}
module Web.Google.Maps.DistanceMatrix
       ( DistMatrixRequest(..)
       , DistMatrixElement(..)
       , DistMatrixResponse (..)
       , queryDistMatrix
       , defaultDistMatrixRequest
       ) where

import Web.Google.Maps.Internal
import Web.Google.Maps.Types

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (isJust, fromJust)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

data DistMatrixMode = Driving | Walking | Bicycling

instance Show DistMatrixMode where
  show Driving   = "driving"
  show Walking   = "walking"
  show Bicycling = "bicycling"

data DistMatrixAvoid = Tolls | Highways | Ferries

instance Show DistMatrixAvoid where
  show Tolls    = "tolls"
  show Highways = "highways"
  show Ferries  = "ferries"

data DistMatrixUnit = Metric | Imperial

instance Show DistMatrixUnit where
  show Metric   = "metric"
  show Imperial = "imperial"

data DistMatrixRequest =  DistMatrixRequest
  { origins :: [Text]
  , destinations :: [Text]
  , mode :: Maybe DistMatrixMode
  , language :: Maybe Text
  , avoid :: Maybe DistMatrixAvoid
  , unit :: Maybe DistMatrixUnit
  , departureTime :: Maybe Integer
  } deriving (Show)

data DistMatrixElement = DistMatrixElement
  { dmeStatus    :: Text
  , dmeDurValue  :: Int
  , dmeDurText   :: Text
  , dmeDistValue :: Int
  , dmeDistText  :: Text
  } deriving (Show)

instance FromJSON DistMatrixElement where
  parseJSON (Object o) = DistMatrixElement
                         <$> (o .: "status")
                         <*> ((o .: "duration") >>= (.: "value"))
                         <*> ((o .: "duration") >>= (.: "text"))
                         <*> ((o .: "distance") >>= (.: "value"))
                         <*> ((o .: "distance") >>= (.: "text"))
  parseJSON _          = mzero

data DistMatrixResponse = DistMatrixResponse
  { dmrStatus       :: Text
  , dmrOrigins      :: [Text]
  , dmrDestinations :: [Text]
  , dmrMatrix       :: Maybe (Matrix DistMatrixElement)
  } deriving (Show)

instance FromJSON DistMatrixResponse where
    parseJSON (Object o) = do
      status <- o .: "status"
      org <- o .: "origin_addresses"
      dest <- o .: "destination_addresses"
      rows <- (o .: "rows")
      els <- case rows of
                  Array r -> do
                    elements <- V.toList <$> V.mapM (\(Object x) -> x .: "elements") r
                    return elements
                  _ -> mzero
      return (DistMatrixResponse status org dest (Just $ Mat.fromLists els))
    parseJSON _          = mzero

defaultDistMatrixRequest :: [Text]-> [Text] -> DistMatrixRequest
defaultDistMatrixRequest org dest = DistMatrixRequest
  { origins       = org
  , destinations  = dest
  , mode          = Nothing
  , language      = Nothing
  , avoid         = Nothing
  , unit          = Nothing
  , departureTime = Nothing
  }

distMatrixWebService :: GoogleMapsWebService DistMatrixRequest DistMatrixResponse
distMatrixWebService = GoogleMapsWebService "distancematrix" params
  where
    params :: DistMatrixRequest -> [(String, String)]
    params DistMatrixRequest{ .. } = map (\(x,y) -> (x, fromJust y)) $
                                     filter (\(_,y) -> isJust y)
                                      [ ("origins"        , originsMaybe origins)
                                      , ("destinations"   , destMaybe destinations)
                                      , ("mode"           , show `fmap` mode)
                                      , ("language"       , show `fmap` language)
                                      , ("avoid"          , show `fmap` avoid)
                                      , ("unit"           , show `fmap` unit)
                                      , ("departure_time" , show `fmap` departureTime)
                                      ]
    originsMaybe os = Just (foldl (\s e -> s ++ "|" ++ e) "" $ map T.unpack os)
    destMaybe    ds = Just (foldl (\s e -> s ++ "|" ++ e) "" $ map T.unpack ds)

queryDistMatrix :: DistMatrixRequest -> GoogleMaps DistMatrixResponse
queryDistMatrix = queryAPI distMatrixWebService
