{-# LANGUAGE RecordWildCards #-}
module Web.Google.Maps.Services.DistanceMatrix
       ( DMRequest(..)
       , DMElement(..)
       , DMResponse (..)
       , defaultDMRequest
       , webService
       ) where

import Control.Arrow (second)
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (isJust, fromJust)
import Data.Matrix (Matrix)
import qualified Data.Matrix as Mat
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Web.Google.Maps.Types

-- Distance Matrix request data types

data DMMode = Driving | Walking | Bicycling

instance Show DMMode where
  show Driving   = "driving"
  show Walking   = "walking"
  show Bicycling = "bicycling"

data DMAvoid = Tolls | Highways | Ferries

instance Show DMAvoid where
  show Tolls    = "tolls"
  show Highways = "highways"
  show Ferries  = "ferries"

data DMUnit = Metric | Imperial

instance Show DMUnit where
  show Metric   = "metric"
  show Imperial = "imperial"


data DMRequest =  DMRequest
  { origins :: [Text]
  , destinations :: [Text]
  , mode :: Maybe DMMode
  , language :: Maybe Text
  , avoid :: Maybe DMAvoid
  , unit :: Maybe DMUnit
  , departureTime :: Maybe Integer
  } deriving (Show)


-- Distance matrix response data types

data DMEStatus = ElementOk
                             | ElementNotFound
                             | ElementZeroResults
                             deriving (Show)

instance FromJSON DMEStatus where
  parseJSON o = case o of
                  String "OK" -> return ElementOk
                  String "NOT_FOUND" -> return ElementNotFound
                  String "ZERO_RESULTS" -> return ElementZeroResults
                  _ -> mzero

data DMElement = DMElement
  { dmeStatus    :: DMEStatus
  , dmeDurValue  :: Int
  , dmeDurText   :: Text
  , dmeDistValue :: Int
  , dmeDistText  :: Text
  } deriving (Show)

instance FromJSON DMElement where
  parseJSON (Object o) = DMElement
                         <$> (o .: "status")
                         <*> ((o .: "duration") >>= (.: "value"))
                         <*> ((o .: "duration") >>= (.: "text"))
                         <*> ((o .: "distance") >>= (.: "value"))
                         <*> ((o .: "distance") >>= (.: "text"))
  parseJSON _          = mzero

data DistMatrixStatus = Ok
                      | InvalidRequest
                      | MaxElementsExceeded
                      | OverQueryLimit
                      | RequestDenied
                      | UnkownError
                      deriving (Show)

instance FromJSON DistMatrixStatus where
  parseJSON o = case o of
                  String "OK" -> return Ok
                  String "INVALID_REQUEST" -> return InvalidRequest
                  String "MAX_ELEMENTS_EXCEEDED" -> return MaxElementsExceeded
                  String "OVER_QUERY_LIMIT" -> return OverQueryLimit
                  String "REQUEST_DENIED" -> return RequestDenied
                  String "UNKNOWN_ERROR" -> return UnkownError
                  _ -> mzero

data DMResponse = DMResponse
  { dmrStatus       :: DistMatrixStatus
  , dmrOrigins      :: [Text]
  , dmrDestinations :: [Text]
  , dmrMatrix       :: Maybe (Matrix DMElement)
  } deriving (Show)

instance FromJSON DMResponse where
    parseJSON (Object o) = do
      status <- o .: "status"
      org <- o .: "origin_addresses"
      dest <- o .: "destination_addresses"
      rows <- o .: "rows"
      els <- case rows of
                  Array r ->  V.toList <$> V.mapM (\(Object x) -> x .: "elements") r
                  _ -> mzero
      return (DMResponse status org dest (Just $ Mat.fromLists els))
    parseJSON _          = mzero

defaultDMRequest :: [Text]-> [Text] -> DMRequest
defaultDMRequest org dest = DMRequest
  { origins       = org
  , destinations  = dest
  , mode          = Nothing
  , language      = Nothing
  , avoid         = Nothing
  , unit          = Nothing
  , departureTime = Nothing
  }

webService :: WebService DMRequest DMResponse
webService = WebService "distancematrix" params
  where
    params :: DMRequest -> [(String, String)]
    params DMRequest{ .. } = map (second fromJust) $
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