module AesonBenchmark where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified SmartCopy.Formats.JSON as J
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson

import Criterion.Main
import Criterion.Main.Options
import Criterion.Monad
import Criterion.Types

main = defaultMainWith config
     [ bgroup "serialize unvers nested: Some (Spam Int Int)"
       [ bench "smartC-JSON" (whnf J.serializeUnvers Test.some2)
       , bench "Aeson" (whnf Aeson.toJSON Test.some2)
       ]
     , bgroup "serialize unvers nested: Some (Spam Int) Int"
       [ bench "smartC-JSON" (whnf J.serializeUnvers Test.some1)
       , bench "Aeson" (whnf Aeson.toJSON Test.some1)
       ]
     , bgroup "serialize unvers No-Fields types"
       [ bench "smartC-JSON" (whnf J.serializeUnvers Test.bar)
       , bench "Aeson" (whnf Aeson.toJSON Test.bar)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.Bla)
       , bench "Aeson" (whnf Aeson.toJSON Test.Bla)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.mybool)
       , bench "Aeson" (whnf Aeson.toJSON Test.mybool)
       ]
     , bgroup "serialize unvers primitives"
       [ bench "smartC-JSON" (whnf J.serializeUnvers (42 :: Int))
       , bench "Aeson" (whnf Aeson.toJSON (42 :: Int))
       , bench "smartC-JSON" (whnf J.serializeUnvers "Benchmark")
       , bench "Aeson" (whnf Aeson.toJSON "Benchmark")
       , bench "smartC-JSON" (whnf J.serializeUnvers ([1,2,3,4] :: [Int]))
       , bench "Aeson" (whnf Aeson.toJSON ([1,2,3,4] :: [Int]))
       ]
     , bgroup "serialize Array types"
       [ bench "smartC-JSON" (whnf J.serializeUnvers Test.v6)
       , bench "Aeson" (whnf Aeson.toJSON Test.v6)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.v7)
       , bench "Aeson" (whnf Aeson.toJSON Test.v7)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.v8)
       , bench "Aeson" (whnf Aeson.toJSON Test.v8)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.string)
       , bench "Aeson" (whnf Aeson.toJSON Test.string)
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.string')
       , bench "Aeson" (whnf Aeson.toJSON Test.string')
       , bench "smartC-JSON" (whnf J.serializeUnvers Test.booltest')
       , bench "Aeson" (whnf Aeson.toJSON Test.booltest')
       ]
     ]
     where config = defaultConfig
                  { csvFile = Just "AesonBenchmark.csv" }
