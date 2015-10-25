module Test.Geocode where

import Data.Aeson
import Data.Either
import qualified Data.ByteString.Char8 as C8
import Test.Hspec

import Web.Google.Maps.Services.Geocode
import Test.GeocodeData

tests :: Spec
tests =
    describe "Response tests" $ do
        let result :: Either String GeocodeResponse
            result = eitherDecodeStrict . C8.pack $ okResponse
        it "can be parsed" $
          result `shouldSatisfy` isRight
        it "has status ok" $
          result `shouldSatisfy` statusOk
        it "has seven address components" $
          result `shouldSatisfy` addressCompLen7

addressCompLen7 :: Either String GeocodeResponse -> Bool
addressCompLen7 (Left _)  = False
addressCompLen7 (Right r) = (length . grAddressComponents . head .
                             grResults $ r) == 7

statusOk :: Either String GeocodeResponse -> Bool
statusOk (Left _)  = False
statusOk (Right r) = grStatus r == Ok
