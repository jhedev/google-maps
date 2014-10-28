module Web.Google.Maps.Util where

import Data.Aeson.TH (Options(..), defaultOptions)
import Data.Char (isUpper, toLower)

camelCaseToUnderscore :: String -> String
camelCaseToUnderscore = foldl replaceUpper ""
  where
    replaceUpper s c = s ++ if isUpper c && (not . null) s
                                then ['_', toLower c]
                                else [toLower c]

dropToLower :: Int -> Options
dropToLower n = defaultOptions{fieldLabelModifier = map toLower . drop n}

dropCamlCase :: Int -> Options
dropCamlCase n = defaultOptions{fieldLabelModifier = camelCaseToUnderscore . drop n}
