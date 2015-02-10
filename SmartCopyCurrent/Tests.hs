{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances (fromOk)
import SmartCopy

import qualified JSON as J
import qualified SmartBinary as SB
import qualified StringFormat as S
import qualified XmlLikeFormat as X
import qualified TestInstances as Test

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Hashable
import Data.Scientific
import Test.HUnit
import Test.QuickCheck

import qualified Data.HashMap as M
import qualified Data.Serialize as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Aeson as Json
-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative

----------
-- Generate arbitrary JSON values, possibly needed for later testing
----------
instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary Scientific where
    arbitrary = scientific <$> arbitrary <*> arbitrary

instance (Eq k, Ord k, Hashable k, Arbitrary k, Arbitrary v) => Arbitrary (M.HashMap k v) where
    arbitrary = M.fromList <$> arbitrary

instance (Arbitrary a) => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary

instance Arbitrary Json.Value where
    arbitrary = arbitraryValue 3


arbitraryValue :: Int -> Gen Json.Value
arbitraryValue 0 = return Json.Null
arbitraryValue i
    = oneof
    [ Json.object <$> shortListOf ((,) <$> arbitrary <*> subValue)
    , Json.Array . V.fromList <$> shortListOf subValue
    , Json.String <$> arbitrary
    , Json.Number <$> arbitrary
    , Json.Bool <$> arbitrary
    , pure Json.Null
    ]
    where 
        subValue = arbitraryValue (i-1)
        shortListOf = fmap (take 3) . listOf

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

tests_JSON
      = TestList $
        [ TestCase $ 
             do pResult :: Test.MyDouble <- fromOk $ J.parseSmart  Test.v4
                let sResult = J.serializeSmart  pResult
                assertEqual "Parsing JSON" Test.v4 sResult
        , TestCase $ 
             do pResult :: Test.FooBar <- fromOk $ J.parseSmart  Test.v6
                let sResult = J.serializeSmart  pResult
                assertEqual "Parsing JSON" Test.v6 sResult
        , TestCase $ 
             do pResult :: Test.FooBar <- fromOk $ J.parseSmart  Test.v5 
                let sResult = J.serializeSmart  pResult
                assertEqual "Parsing JSON" Test.v5 sResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v9
                pResult :: Test.Bla <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v9 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v10
                pResult :: Test.ArrType <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v10 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v11
                pResult :: Test.ArrTypeBar <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v11 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v12
                pResult :: Test.ArrTypeFooBar <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v12 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.string
                pResult :: Test.StringTest <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.string pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v8
                pResult :: Test.FooBar <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v8 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v7
                pResult :: Test.FooBar <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v7 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v1
                pResult :: Test.Foo <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v1 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.v2
                pResult :: Test.Foo <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.v2 pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.some1
                pResult :: Test.Some <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as JSON Value" Test.some1 pResult
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.some2
                let sResult2 = Json.toJSON Test.some2
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.some1
                let sResult2 = Json.toJSON Test.some1
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v8
                let sResult2 = Json.toJSON Test.v8
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v7
                let sResult2 = Json.toJSON Test.v7
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v1
                let sResult2 = Json.toJSON Test.v1
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult = J.serializeSmart  Test.booltest
                pResult :: Test.BoolTest <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest pResult
        , TestCase $
             do let sResult = J.serializeSmart  Test.booltest'
                pResult :: Test.BoolTest' <- fromOk $ J.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest' pResult
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v2
                let sResult2 = Json.toJSON Test.v2
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v10
                let sResult2 = Json.toJSON Test.v10
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v11
                let sResult2 = Json.toJSON Test.v11
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.v12
                let sResult2 = Json.toJSON Test.v12
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.string
                let sResult2 = Json.toJSON Test.string
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.string'
                let sResult2 = Json.toJSON Test.string'
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.booltest
                let sResult2 = Json.toJSON Test.booltest
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.booltest'
                let sResult2 = Json.toJSON Test.booltest'
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = J.serializeSmart  Test.bar
                let sResult2 = Json.toJSON Test.bar
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do pResult1 :: Test.MyDouble <- fromOk $ J.parseSmart  Test.v3
                let Json.Success pResult2 = Json.fromJSON Test.v3
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.MyDouble <- fromOk $ J.parseSmart  Test.v4
                let Json.Success pResult2 = Json.fromJSON Test.v4
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ J.parseSmart  Test.v5
                let Json.Success pResult2 = Json.fromJSON Test.v5
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ J.parseSmart  Test.v6
                let Json.Success pResult2 = Json.fromJSON Test.v6
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ J.parseSmart  Test.v6
                let Json.Success pResult2 = Json.fromJSON Test.v6
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
         ]

-------------------------------------------------------------------------------
-- String-Encoding
-------------------------------------------------------------------------------

tests_String
      = TestList $
        [ TestCase $ 
             do let sResult = S.serializeSmart  Test.v9
                pResult :: Test.Bla <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v9 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v10
                pResult :: Test.ArrType <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v10 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v11
                pResult :: Test.ArrTypeBar <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v11 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v12
                pResult :: Test.ArrTypeFooBar <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v12 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.some2
                pResult :: Test.Some' <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.some2 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v8
                pResult :: Test.FooBar <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v8 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v7
                pResult :: Test.FooBar <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v7 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v1
                pResult :: Test.Foo <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v1 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v2
                pResult :: Test.Foo <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v2 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.some1
                pResult :: Test.Some <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.some1 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.v9
                pResult :: Test.Bla <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.v9 pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.string
                pResult :: Test.StringTest <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.string pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.string'
                pResult :: Test.StringTest' <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as String'>" Test.string' pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.booltest
                pResult :: Test.BoolTest <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest pResult
        , TestCase $
             do let sResult = S.serializeSmart  Test.booltest'
                pResult :: Test.BoolTest' <- fromOk $ S.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest' pResult
        , TestCase $
             do pResult :: Test.Easy <- fromOk $ S.parseSmart  Test.s1
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s1) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ S.parseSmart  Test.s2
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s2) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ S.parseSmart  Test.s3
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s3) sResult
        , TestCase $
             do pResult :: Test.ArrType <- fromOk $ S.parseSmart  Test.s4
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s4) sResult
        , TestCase $
             do pResult :: Test.ArrTypeBar <- fromOk $ S.parseSmart  Test.s5
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s5) sResult
        , TestCase $
             do pResult :: Test.ArrTypeBar <- fromOk $ S.parseSmart  Test.s6
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s6) sResult
        , TestCase $
             do pResult :: Test.ArrTypeFooBar <- fromOk $ S.parseSmart  Test.s7
                let sResult = filter (/= ' ') $ S.serializeSmart  pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s7) sResult
        ]

-------------------------------------------------------------------------------
-- Xml
-------------------------------------------------------------------------------

tests_Xml
      = TestList $
        [ TestCase $ 
             do let sResult = X.serializeSmart  Test.v9
                pResult :: Test.Bla <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v9 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.string
                pResult :: Test.StringTest <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.string pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.string'
                pResult :: Test.StringTest' <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.string' pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v10
                pResult :: Test.ArrType <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v10 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v11
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v11 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v12
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v12 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.some2
                pResult :: Test.Some' <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.some2 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v8
                pResult :: Test.FooBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v8 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v7
                pResult :: Test.FooBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v7 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v1
                pResult :: Test.Foo <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v1 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v2
                pResult :: Test.Foo <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v2 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.some1
                pResult :: Test.Some <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.some1 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v11
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v11 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.v12
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.v12 pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.booltest
                pResult :: Test.BoolTest <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest pResult
        , TestCase $
             do let sResult = X.serializeSmart  Test.booltest'
                pResult :: Test.BoolTest' <- fromOk $ X.parseSmart  sResult
                assertEqual "Serializing as Xml>" Test.booltest' pResult
        , TestCase $
             do pResult :: Test.Easy <- fromOk $ X.parseSmart  Test.xml1
                let sResult = filter (/= ' ') $ X.serializeSmart  pResult
                assertEqual "Parsing Xml" (filter (/= ' ') Test.xml1) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ X.parseSmart  Test.xml2
                let sResult = filter (/= ' ') $ X.serializeSmart  pResult
                assertEqual "Parsing Xml" (filter (/= ' ') Test.xml2) sResult
        , TestCase $
             do pResult :: Test.Bla <- fromOk $ X.parseSmart  Test.xml3
                let sResult = filter (/= ' ') $ X.serializeSmart  pResult
                assertEqual "Parsing Xml" (filter (/= ' ') Test.xml3) sResult
        , TestCase $
             do pResult :: Test.Some <- fromOk $ X.parseSmart  Test.xml4
                let sResult = filter (/= ' ') $ X.serializeSmart  pResult
                assertEqual "Parsing Xml" (filter (/= ' ') Test.xml4) sResult
        , TestCase $
             do pResult :: Test.Some' <- fromOk $ X.parseSmart  Test.xml5
                let sResult = filter (/= ' ') $ X.serializeSmart  pResult
                assertEqual "Parsing Xml" (filter (/= ' ') Test.xml5) sResult
        ]

-------------------------------------------------------------------------------
-- Binary
-------------------------------------------------------------------------------

tests_Binary
      = TestList $
        [ TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v9
                    sResult2 = B.encode Test.v9
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v10
                    sResult2 = B.encode Test.v10
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2

        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.some1
                    sResult2 = B.encode Test.some1
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.some2
                    sResult2 = B.encode Test.some2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.string
                    sResult2 = B.encode Test.string
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.string'
                    sResult2 = B.encode Test.string'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.booltest
                    sResult2 = B.encode Test.booltest
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.booltest'
                    sResult2 = B.encode Test.booltest'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.mybool
                    sResult2 = B.encode Test.mybool
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.mybool'
                    sResult2 = B.encode Test.mybool'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v1
                    sResult2 = B.encode Test.v1
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2

        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v2
                    sResult2 = B.encode Test.v2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v8
                    sResult2 = B.encode Test.v8
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v7
                    sResult2 = B.encode Test.v7
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v2
                    sResult2 = B.encode Test.v2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v11
                    sResult2 = B.encode Test.v11
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $ 
             do let sResult1 = SB.serializeSmart Test.v12
                    sResult2 = B.encode Test.v12
                assertEqual "Comparing serialized binary with Data.Serialized" sResult1 sResult2
        , TestCase $
             do let sResult = SB.serializeSmart Test.v9
                pResult :: Test.Bla <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v9 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.mybool
                pResult :: Test.MyBool <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.mybool pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.mybool'
                pResult :: Test.MyBool <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.mybool' pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.bar
                pResult :: Test.Bar <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.bar pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.v11
                pResult :: Test.ArrTypeBar <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v11 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.some1
                pResult :: Test.Some <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.some1 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.some2
                pResult :: Test.Some' <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.some2 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.string
                pResult :: Test.StringTest <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.string pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.v10
                pResult :: Test.ArrType <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v10 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.v12
                pResult :: Test.ArrTypeFooBar <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v12 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.v8
                pResult :: Test.FooBar <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v8 pResult
        , TestCase $
             do let sResult = SB.serializeSmart Test.v7
                pResult :: Test.FooBar <- either (\a -> fail $ msg a) return (SB.parseSmart sResult)
                assertEqual "Serializing as binary" Test.v7 pResult
        ]
        where msg a = "Failure: " ++ a


main = do runTestTT tests_JSON
          runTestTT tests_String
          runTestTT tests_Xml
          runTestTT tests_Binary
