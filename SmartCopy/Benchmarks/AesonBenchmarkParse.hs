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
     [ bgroup "Parse unversioned ADTs"
       [ bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.Some2) (J.serializeUnvers Test.some2))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.Some2)
         (Aeson.toJSON Test.some2))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.Some) (J.serializeUnvers Test.some1))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.Some)
         (Aeson.toJSON Test.some1))
       ]
     , bgroup "Parse unversioned empty-constructor types"
       [ bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.Bar) (J.serializeUnvers Test.bar))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.Bar)
         (Aeson.toJSON Test.bar))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.Bla) (J.serializeUnvers Test.Bla))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.Bla)
         (Aeson.toJSON Test.Bla))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.MyBool) (J.serializeUnvers Test.mybool))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.MyBool)
         (Aeson.toJSON Test.mybool))
       ]
     , bgroup "Parse unversioned primitives"
       [ bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Int) (J.serializeUnvers (42 :: Int)))
       , bench "Aeson" (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Int)
         (Aeson.toJSON (42 :: Int)))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail String) (J.serializeUnvers "Benchmark"))
       , bench "Aeson" (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result String)
         (Aeson.toJSON "Benchmark"))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail [Int]) (J.serializeUnvers ([1,2,3,4] :: [Int])))
       , bench "Aeson" (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result [Int])

         (Aeson.toJSON ([1,2,3,4] :: [Int])))
       ]
     , bgroup "Parse unversioned Array types"
       [ bench "SmartCopy-JSON" 
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.ArrType) (J.serializeUnvers Test.v6a))
       , bench "Aeson" (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.ArrType)
         (Aeson.toJSON Test.v6a))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.ArrTypeBar) (J.serializeUnvers Test.v7))
       , bench "Aeson" (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.ArrTypeBar)

         (Aeson.toJSON Test.v7))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.ArrTypeFooBar)
         (J.serializeUnvers Test.v8))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.ArrTypeFooBar)
         (Aeson.toJSON Test.v8))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.StringTest)
         (J.serializeUnvers Test.string))
       , bench "Aeson" 
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.StringTest)
         (Aeson.toJSON Test.string))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.StringTest2)
         (J.serializeUnvers Test.string'))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.StringTest2)
         (Aeson.toJSON Test.string'))
       , bench "SmartCopy-JSON"
         (whnf (J.parseUnvers :: Aeson.Value -> Fail Test.BoolTestLong)
         (J.serializeUnvers Test.booltest'))
       , bench "Aeson"
         (whnf (Aeson.fromJSON :: Aeson.Value -> Aeson.Result Test.BoolTestLong)
         (Aeson.toJSON Test.booltest'))
       ]
     ]
