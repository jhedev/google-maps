module Main where

import           Test.Hspec
import qualified Test.Geocode    as Geo
import qualified Test.DistMatrix as Dist

main :: IO ()
main = do
    hspec $ do
      Geo.tests
      Dist.tests
