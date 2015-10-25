module Test.DistMatrix where

import Data.Aeson
import Data.Either
import qualified Data.ByteString.Char8 as C8
import Test.Hspec

import Web.Google.Maps.Services.DistanceMatrix
import Test.DistMatrixData

tests :: Spec
tests =
    describe "Response tests" $ do
        let result :: Either String DMResponse
            result = eitherDecodeStrict . C8.pack $ okResponse
        it "can be parsed" $
          result `shouldSatisfy` isRight
        it "has status ok" $
          result `shouldSatisfy` statusOk

statusOk :: Either String DMResponse -> Bool
statusOk (Left _)  = False
statusOk (Right r) = dmrStatus r == Ok
