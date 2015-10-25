{-# LANGUAGE QuasiQuotes #-}
module Test.DistMatrixData where

import Text.RawString.QQ

okResponse :: String
okResponse = [r|
{
   "destination_addresses" : [ "Berlin, Germany", "Hamburg, Germany" ],
   "origin_addresses" : [ "Aachen, Germany", "Cologne, Germany" ],
   "rows" : [
      {
         "elements" : [
            {
               "distance" : {
                  "text" : "660 km",
                  "value" : 659541
               },
               "duration" : {
                  "text" : "1 day 10 hours",
                  "value" : 124078
               },
               "status" : "OK"
            },
            {
               "distance" : {
                  "text" : "482 km",
                  "value" : 481956
               },
               "duration" : {
                  "text" : "1 day 1 hour",
                  "value" : 89564
               },
               "status" : "OK"
            }
         ]
      },
      {
         "elements" : [
            {
               "distance" : {
                  "text" : "600 km",
                  "value" : 599672
               },
               "duration" : {
                  "text" : "1 day 8 hours",
                  "value" : 113885
               },
               "status" : "OK"
            },
            {
               "distance" : {
                  "text" : "435 km",
                  "value" : 435214
               },
               "duration" : {
                  "text" : "22 hours 45 mins",
                  "value" : 81925
               },
               "status" : "OK"
            }
         ]
      }
   ],
   "status" : "OK"
}
|]
