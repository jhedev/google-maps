module Main where

import           Test.Hspec
import qualified Test.Geocode as Geo

main :: IO ()
main = do
    hspec $ do
      Geo.tests
