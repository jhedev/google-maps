{-# LANGUAGE RecordWildCards #-}
module Web.Google.Maps.Geocode
       ( GeocodeRequest(..)
       , ComponentFilter(..)
       , AddressComponent(..)
       , LatLong(..)
       , ViewPort(..)
       , Geometry(..)
       , GeocodeResult(..)
       , GeocodeResponse(..)
       , queryGeocode
       ) where

import Web.Google.Maps.Internal
import Web.Google.Maps.Types
import Web.Google.Maps.Util

import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T

data ComponentFilter = Route Text
                     | Locality Text
                     | AdministrativeArea Text
                     | PostalCode Text
                     | Country Text

instance Show ComponentFilter where
  show f = case f of
    (Route r)              -> "route:" ++ T.unpack r
    (Locality l)           -> "locality:" ++ T.unpack l
    (AdministrativeArea a) -> "administrative_area:" ++ T.unpack a
    (PostalCode p)         -> "postal_code:" ++ T.unpack p
    (Country c)            -> "country:" ++ T.unpack c

-- Geocode Requests
data GeocodeRequest = GeocodeRequest
  { address    :: Text
  , components :: [ComponentFilter]
  , gcrbounds     :: Maybe Text
  , gcrlanguage   :: Maybe Text
  , region     :: Maybe Text
  } deriving (Show)

data AddressComponent = AddressComponent
  { acLongName :: Text
  , acShortName :: Text
  , acTypes :: [Text]
  } deriving (Show)

$(deriveJSON (dropCamlCase 2) ''AddressComponent)

data LatLong = LatLong
  { lat :: Double
  , lng :: Double
  } deriving (Show)

$(deriveJSON defaultOptions ''LatLong)

data ViewPort = ViewPort
  { vpNortheast :: LatLong
  , vpSouthwest :: LatLong
  } deriving (Show)

$(deriveJSON (dropToLower 2) ''ViewPort)

data Geometry = Geometry
  { geoLocation :: LatLong
  , geoLocationType :: Text
  , geoViewport :: ViewPort
  } deriving (Show)

$(deriveJSON (dropCamlCase 3) ''Geometry)

data GeocodeResult = GeocodeResult
  { grAddressComponents :: [AddressComponent]
  , grFormattedAddress :: Text
  , grGeometry :: Geometry
  , grTypes :: [Text]
  } deriving (Show)

$(deriveJSON (dropCamlCase 2) ''GeocodeResult)

data GeocodeResponse = GeocodeResponse
  { grResults :: [GeocodeResult]
  , grStatus :: Text
  } deriving (Show)

$(deriveJSON (dropToLower 2) ''GeocodeResponse)

geocodeWebService :: GoogleMapsWebService GeocodeRequest GeocodeResponse
geocodeWebService = GoogleMapsWebService "geocode" params
  where
    params GeocodeRequest{ .. } = [ ("address", T.unpack address)
                                  , ("components", foldl (\s e -> s ++ "|" ++ e) "" $ map show components)
                                  ]

queryGeocode :: GeocodeRequest -> GoogleMaps GeocodeResponse
queryGeocode = queryAPI geocodeWebService
