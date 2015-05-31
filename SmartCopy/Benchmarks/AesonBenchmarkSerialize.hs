-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.Aeson as Aeson
import qualified Data.SmartCopy.Formats.JSON as J

import Criterion.Main
import Criterion.Monad
import Criterion.Types
import Data.SmartCopy

main = defaultMain
     [ bgroup "Serialize unversioned ADTs: Some (Spam Int Int)"
       [ bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.some2)
       , bench "Aeson" (whnf Aeson.toJSON Test.some2)
       ]
     , bgroup "Serialize unversioned ADTs: Some (Spam Int) Int"
       [ bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.some1)
       , bench "Aeson" (whnf Aeson.toJSON Test.some1)
       ]
     , bgroup "Serialize unversioned empty-constructor types"
       [ bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.bar)
       , bench "Aeson" (whnf Aeson.toJSON Test.bar)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.Bla)
       , bench "Aeson" (whnf Aeson.toJSON Test.Bla)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.mybool)
       , bench "Aeson" (whnf Aeson.toJSON Test.mybool)
       ]
     , bgroup "Serialize unversioned primitives"
       [ bench "SmartCopy-JSON" (whnf J.serializeUnvers (42 :: Int))
       , bench "Aeson" (whnf Aeson.toJSON (42 :: Int))
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers "Benchmark")
       , bench "Aeson" (whnf Aeson.toJSON "Benchmark")
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers ([1,2,3,4] :: [Int]))
       , bench "Aeson" (whnf Aeson.toJSON ([1,2,3,4] :: [Int]))
       ]
     , bgroup "serialize unversioned Array types"
       [ bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.v6a)
       , bench "Aeson" (whnf Aeson.toJSON Test.v6a)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.v7)
       , bench "Aeson" (whnf Aeson.toJSON Test.v7)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.v8)
       , bench "Aeson" (whnf Aeson.toJSON Test.v8)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.string)
       , bench "Aeson" (whnf Aeson.toJSON Test.string)
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.string')
       , bench "Aeson" (whnf Aeson.toJSON Test.string')
       , bench "SmartCopy-JSON" (whnf J.serializeUnvers Test.booltest')
       , bench "Aeson" (whnf Aeson.toJSON Test.booltest')
       ]
     ]
