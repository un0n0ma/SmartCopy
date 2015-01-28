{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import JSON
import MonadTypesInstances (fromOk)
import SmartCopy
import qualified TestInstances as Test

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Hashable
import Data.Scientific
import Test.HUnit
import Test.QuickCheck
import qualified Data.HashMap as M
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


tests_JSON
      = TestList $
        [ TestCase $ 
             do pResult :: Test.MyDouble <- fromOk $ parseSmart jsonParseFormat Test.v4
                let sResult = serializeSmart jsonSerializationFormat pResult
                assertEqual "Parsing JSON" Test.v4 sResult
        , TestCase $ 
             do pResult :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat Test.v6
                let sResult = serializeSmart jsonSerializationFormat pResult
                assertEqual "Parsing JSON" Test.v6 sResult
        , TestCase $ 
             do pResult :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat Test.v5 
                let sResult = serializeSmart jsonSerializationFormat pResult
                assertEqual "Parsing JSON" Test.v5 sResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.v9
                pResult :: Test.Bla <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.v9 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.v8
                pResult :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.v8 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.v7
                pResult :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.v7 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.v1
                pResult :: Test.Foo <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.v1 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.v2
                pResult :: Test.Foo <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.v2 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.some1
                pResult :: Test.Some <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.some1 pResult
        , TestCase $
             do let sResult = serializeSmart jsonSerializationFormat Test.some2
                pResult :: Test.Some' <- fromOk $ parseSmart jsonParseFormat sResult
                assertEqual "Serializing JSON" Test.some2 pResult
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.some2
                let sResult2 = Json.toJSON Test.some2
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.some1
                let sResult2 = Json.toJSON Test.some1
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.v8
                let sResult2 = Json.toJSON Test.v8
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.v7
                let sResult2 = Json.toJSON Test.v7
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.v1
                let sResult2 = Json.toJSON Test.v1
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do let sResult1 = serializeSmart jsonSerializationFormat Test.v2
                let sResult2 = Json.toJSON Test.v2
                assertEqual "Comparing serialized with Aeson" sResult1 sResult2
        , TestCase $
             do pResult1 :: Test.MyDouble <- fromOk $ parseSmart jsonParseFormat Test.v3
                let Json.Success pResult2 = Json.fromJSON Test.v3
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.MyDouble <- fromOk $ parseSmart jsonParseFormat Test.v4
                let Json.Success pResult2 = Json.fromJSON Test.v4
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat Test.v5
                let Json.Success pResult2 = Json.fromJSON Test.v5
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ parseSmart jsonParseFormat Test.v6
                let Json.Success pResult2 = Json.fromJSON Test.v6
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
         ]

main = runTestTT tests_JSON
