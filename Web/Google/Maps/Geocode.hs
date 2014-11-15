{-# LANGUAGE RecordWildCards #-}
module Web.Google.Maps.Geocode
       ( GeocodeRequest(..)
       , ComponentFilter(..)
       , AddressType(..)
       , AddressComponent(..)
       , LatLong(..)
       , ViewPort(..)
       , Geometry(..)
       , GeocodeStatus(..)
       , GeocodeResult(..)
       , GeocodeResponse(..)
       , defaultGeocodeRequest
       , queryGeocode
       ) where

import Web.Google.Maps.Internal
import Web.Google.Maps.Types
import Web.Google.Maps.Util

import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T

data ComponentFilter = RouteFilter Text
                     | LocalityFilter Text
                     | AdministrativeAreaFilter Text
                     | PostalCodeFilter Text
                     | CountryFilter Text

instance Show ComponentFilter where
  show f = case f of
    (RouteFilter r)              -> "route:" ++ T.unpack r
    (LocalityFilter l)           -> "locality:" ++ T.unpack l
    (AdministrativeAreaFilter a) -> "administrative_area:" ++ T.unpack a
    (PostalCodeFilter p)         -> "postal_code:" ++ T.unpack p
    (CountryFilter c)            -> "country:" ++ T.unpack c

-- Geocode request types
data GeocodeRequest = GeocodeRequest
  { address    :: Text
  , components :: [ComponentFilter]
  , gcrbounds     :: Maybe Text
  , gcrlanguage   :: Maybe Text
  , region     :: Maybe Text
  } deriving (Show)

defaultGeocodeRequest :: Text -> GeocodeRequest
defaultGeocodeRequest addr = GeocodeRequest
  { address     = addr
  , components  = []
  , gcrbounds   = Nothing
  , gcrlanguage = Nothing
  , region      = Nothing
  }

-- Geocode response types

data AddressType = StreetAddress
                 | Route
                 | Intersection
                 | Political
                 | Country
                 | AdministrativeAreaLvl1
                 | AdministrativeAreaLvl2
                 | AdministrativeAreaLvl3
                 | AdministrativeAreaLvl4
                 | AdministrativeAreaLvl5
                 | ColloquialArea
                 | Locality
                 | Ward
                 | Sublocality
                 | SublocalityLvl1
                 | SublocalityLvl2
                 | SublocalityLvl3
                 | SublocalityLvl4
                 | SublocalityLvl5
                 | Neighborhood
                 | Premise
                 | Subpremise
                 | PostalCode
                 | NaturalFeature
                 | Airport
                 | Park
                 | PointOfInterest
                 | Floor
                 | Establishment
                 | Parking
                 | PostBox
                 | PostalTown
                 | Room
                 | StreetNumber
                 | BusStation
                 | TrainStation
                 | TransitStation
                 | OtherType Text
                 deriving (Show)

instance FromJSON AddressType where
  parseJSON o = case o of
                  String "street_address"              -> return StreetAddress
                  String "route"                       -> return Route
                  String "intersection"                -> return Intersection
                  String "political"                   -> return Political
                  String "country"                     -> return Country
                  String "administrative_area_level_1" -> return AdministrativeAreaLvl1
                  String "administrative_area_level_2" -> return AdministrativeAreaLvl2
                  String "administrative_area_level_3" -> return AdministrativeAreaLvl3
                  String "administrative_area_level_4" -> return AdministrativeAreaLvl4
                  String "administrative_area_level_5" -> return AdministrativeAreaLvl5
                  String "colloquial_area"             -> return ColloquialArea
                  String "locality"                    -> return Locality
                  String "ward"                        -> return Ward
                  String "sublocality"                 -> return Sublocality
                  String "sublocality_level_1"         -> return SublocalityLvl1
                  String "sublocality_level_2"         -> return SublocalityLvl2
                  String "sublocality_level_3"         -> return SublocalityLvl3
                  String "sublocality_level_4"         -> return SublocalityLvl4
                  String "sublocality_level_5"         -> return SublocalityLvl5
                  String "neighborhood"                -> return Neighborhood
                  String "permise"                     -> return Premise
                  String "subpremise"                  -> return Subpremise
                  String "postal_code"                 -> return PostalCode
                  String "natural_feature"             -> return NaturalFeature
                  String "airport"                     -> return Airport
                  String "park"                        -> return Park
                  String "point_of_interest"           -> return PointOfInterest
                  String "floor"                       -> return Floor
                  String "establishment"               -> return Establishment
                  String "parking"                     -> return Parking
                  String "post_box"                    -> return PostBox
                  String "postal_town"                 -> return PostalTown
                  String "room"                        -> return Room
                  String "street_number"               -> return StreetNumber
                  String "bus_station"                 -> return BusStation
                  String "train_station"               -> return TrainStation
                  String "transit_station"             -> return TransitStation
                  String s                             -> return $ OtherType s
                  _                                    -> mzero

data AddressComponent = AddressComponent
  { acLongName :: Text
  , acShortName :: Text
  , acTypes :: [AddressType]
  } deriving (Show)

$(deriveFromJSON (dropCamlCase 2) ''AddressComponent)

data LatLong = LatLong
  { lat :: Double
  , lng :: Double
  } deriving (Show)

$(deriveFromJSON defaultOptions ''LatLong)

data ViewPort = ViewPort
  { vpNortheast :: LatLong
  , vpSouthwest :: LatLong
  } deriving (Show)

$(deriveFromJSON (dropToLower 2) ''ViewPort)

data Geometry = Geometry
  { geoLocation :: LatLong
  , geoLocationType :: Text
  , geoViewport :: ViewPort
  } deriving (Show)

$(deriveFromJSON (dropCamlCase 3) ''Geometry)

data GeocodeResult = GeocodeResult
  { grAddressComponents :: [AddressComponent]
  , grFormattedAddress :: Text
  , grGeometry :: Geometry
  , grTypes :: [AddressType]
  } deriving (Show)

$(deriveFromJSON (dropCamlCase 2) ''GeocodeResult)

data GeocodeStatus = Ok
                   | ZeroResults
                   | OverQueryLimit
                   | RequestDenied
                   | InvalidRequest
                   | UnkownError
                   deriving (Show)

instance FromJSON GeocodeStatus where
  parseJSON o = case o of
                  String "OK" -> return Ok
                  String "ZERO_RESULTS" -> return ZeroResults
                  String "OVER_QUERY_LIMIT" -> return OverQueryLimit
                  String "REQUEST_DENIED" -> return RequestDenied
                  String "INVLAID_REQUEST" -> return InvalidRequest
                  String "UNKOWN_ERROR" -> return UnkownError
                  _ -> mzero

data GeocodeResponse = GeocodeResponse
  { grResults :: [GeocodeResult]
  , grStatus :: GeocodeStatus
  } deriving (Show)

$(deriveFromJSON (dropToLower 2) ''GeocodeResponse)

geocodeWebService :: GoogleMapsWebService GeocodeRequest GeocodeResponse
geocodeWebService = GoogleMapsWebService "geocode" params
  where
    params GeocodeRequest{ .. } = [ ("address", T.unpack address)
                                  , ("components", foldl (\s e -> s ++ "|" ++ e) "" $ map show components)
                                  ]

queryGeocode :: GeocodeRequest -> GoogleMaps GeocodeResponse
queryGeocode = queryAPI geocodeWebService
