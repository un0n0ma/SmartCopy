{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.MonadTypesInstances (fromOk)
import SmartCopy.SmartCopy

import qualified SmartCopy.Formats.JSON as J
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified SmartCopy.Formats.SafeCopy as SMC
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified SmartCopy.Formats.StringFormat as S
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified SmartCopy.Formats.XmlLikeFormat as X
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesDerived as GTest
import qualified Tests.TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Hashable
import Data.Scientific
import System.Environment
import Test.HUnit
import Test.QuickCheck
import Hexdump

import qualified Data.HashMap as M
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Aeson as Json
-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------
tests_JSON_vers =
        TestList $
        [ TestCase $ 
             do pResult :: Test.Easy <- fromOk $ J.parseSmart Test.js1
                let sResult = J.serializeSmart pResult
                assertEqual "Parsing versioned JSON Values" Test.js1 sResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v5
                pResult :: Test.Bla <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v5 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v4
                pResult :: Test.FooBar <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v4 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v3
                pResult :: Test.FooBar <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v3 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v6
                pResult :: Test.ArrType <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v6 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v7 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v8 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v1
                pResult :: Test.Foo <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v1 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.v2
                pResult :: Test.Foo <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.v2 pResult
        , TestCase $ --9
             do let sResult = J.serializeSmart Test.some1
                pResult :: Test.Some <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.some1 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.some2 --10
                pResult :: Test.Some2 <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.some2 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.mybool
                pResult :: Test.MyBool <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.mybool pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.mybool'
                pResult :: Test.MyBool <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.mybool' pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.bar
                pResult :: Test.Bar <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.bar pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.string
                pResult :: Test.StringTest <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.string pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.string' --15
                pResult :: Test.StringTest2 <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.string' pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.booltest
                pResult :: Test.BoolTest <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.booltest pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.booltest' pResult
        , TestCase $
             do let sResult = J.serializeSmart TestV2.easy
                pResult :: TestV2.EasyV2 <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" TestV2.easy pResult
        , TestCase $
             do let sResult = J.serializeSmart TestV2.some
                pResult :: TestV2.SomeV2 <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" TestV2.some pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.some1
                pResult <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.some1 pResult
        , TestCase $
             do let sResult = J.serializeSmart ([1,2,3,4] :: [Int])
                pResult <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" ([1,2,3,4] :: [Int]) pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.maybetest1
                pResult <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.maybetest1 pResult
        , TestCase $
             do let sResult = J.serializeSmart Test.maybetest2
                pResult <- fromOk $ J.parseSmart sResult
                assertEqual "Serializing as versioned JSON Value" Test.maybetest2 pResult
        ]

tests_JSON_unvers
      = TestList $
        [ TestCase $ 
             do pResult :: Test.MyDouble <- fromOk $ J.parseUnvers Test.js2
                let sResult = J.serializeUnvers pResult
                assertEqual "Parsing JSON" Test.js2 sResult
        , TestCase $ 
             do pResult :: Test.FooBar <- fromOk $ J.parseUnvers Test.js3
                let sResult = J.serializeUnvers pResult
                assertEqual "Parsing JSON" Test.js3 sResult
        , TestCase $
             do let sResult = J.serializeUnvers  Test.v5
                pResult :: Test.Bla <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v5 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v4
                pResult :: Test.FooBar <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v4 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v3
                pResult :: Test.FooBar <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as JSON Value" Test.v3 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v6
                pResult :: Test.ArrType <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v6 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v7 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as JSON Value" Test.v8 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.string
                pResult :: Test.StringTest <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.string pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.string'
                pResult :: Test.StringTest2 <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as JSON Value" Test.string' pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v1
                pResult :: Test.Foo <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v1 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.v2
                pResult :: Test.Foo <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON Value" Test.v2 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.some1
                pResult :: Test.Some <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as JSON Value" Test.some1 pResult
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.some2
                let sResult2 = Json.toJSON Test.some2
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.some1
                let sResult2 = Json.toJSON Test.some1
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v4
                let sResult2 = Json.toJSON Test.v4
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v3
                let sResult2 = Json.toJSON Test.v3
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v1
                let sResult2 = Json.toJSON Test.v1
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult = J.serializeUnvers Test.booltest
                pResult :: Test.BoolTest <- fromOk $ J.parseUnvers  sResult
                assertEqual "Serializing as JSON" Test.booltest pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as JSON" Test.booltest' pResult
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v2
                let sResult2 = Json.toJSON Test.v2
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v6
                let sResult2 = Json.toJSON Test.v6
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v7
                let sResult2 = Json.toJSON Test.v7
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.v8
                let sResult2 = Json.toJSON Test.v8
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.string
                let sResult2 = Json.toJSON Test.string
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.string'
                let sResult2 = Json.toJSON Test.string'
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.booltest
                let sResult2 = Json.toJSON Test.booltest
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.booltest'
                let sResult2 = Json.toJSON Test.booltest'
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do let sResult1 = J.serializeUnvers Test.bar
                let sResult2 = Json.toJSON Test.bar
                assertEqual "Comparing serialized with Aeson" sResult2 sResult1
        , TestCase $
             do pResult1 :: Test.MyDouble <- fromOk $ J.parseUnvers Test.js2
                let Json.Success pResult2 = Json.fromJSON Test.js2
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ J.parseUnvers Test.js3
                let Json.Success pResult2 = Json.fromJSON Test.js3
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do pResult1 :: Test.FooBar <- fromOk $ J.parseUnvers Test.js3
                let Json.Success pResult2 = Json.fromJSON Test.js3
                assertEqual "Comparing parsed with Aeson" pResult1 pResult2
        , TestCase $
             do let sResult = J.serializeUnvers ([1,2,3,4] :: [Int])
                pResult <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as unversioned JSON Value" ([1,2,3,4] :: [Int]) pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.maybetest1
                pResult <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as unversioned JSON Value" Test.maybetest1 pResult
        , TestCase $
             do let sResult = J.serializeUnvers Test.maybetest2
                pResult <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as unversioned JSON Value" Test.maybetest2 pResult
        , TestCase $
             do let sResult = J.serializeUnvers TestV2.easy
                pResult :: TestV2.EasyV2 <- fromOk $ J.parseUnvers sResult
                assertEqual "Serializing as versioned JSON Value" TestV2.easy pResult
         ]

-------------------------------------------------------------------------------
-- String-Encoding
-------------------------------------------------------------------------------

tests_String_unvers
      = TestList $
        [ TestCase $ 
             do let sResult = S.serializeUnvers  Test.v5
                pResult :: Test.Bla <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v5 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v6
                pResult :: Test.ArrType <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v6 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v7 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v8 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.some2
                pResult :: Test.Some2 <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.some2 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v4
                pResult :: Test.FooBar <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v4 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v3
                pResult :: Test.FooBar <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v3 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v1
                pResult :: Test.Foo <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v1 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v2
                pResult :: Test.Foo <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v2 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.some1
                pResult :: Test.Some <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.some1 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.v5
                pResult :: Test.Bla <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.v5 pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.string
                pResult :: Test.StringTest <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.string pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.string'
                pResult :: Test.StringTest2 <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.string' pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.booltest
                pResult :: Test.BoolTest <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.booltest pResult
        , TestCase $
             do let sResult = S.serializeUnvers  Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ S.parseUnvers  sResult
                assertEqual "Serializing as String" Test.booltest' pResult
        , TestCase $
             do pResult :: Test.Easy <- fromOk $ S.parseUnvers Test.s1
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s1) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ S.parseUnvers Test.s2
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s2) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ S.parseUnvers Test.s3
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s3) sResult
        , TestCase $
             do pResult :: Test.ArrType <- fromOk $ S.parseUnvers Test.s4
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s4) sResult
        , TestCase $
             do pResult :: Test.ArrTypeBar <- fromOk $ S.parseUnvers Test.s5
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s5) sResult
        , TestCase $ --20 - what's up here?
             do pResult :: Test.ArrTypeBar <- fromOk $ S.parseUnvers Test.s6
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s6) sResult
        , TestCase $
             do pResult :: Test.ArrTypeFooBar <- fromOk $ S.parseUnvers Test.s7
                let sResult = filter (/= ' ') $ S.serializeUnvers pResult
                assertEqual "Parsing String" (filter (/= ' ') Test.s7) sResult
        , TestCase $
             do let sResult = S.serializeUnvers Test.maybetest1
                pResult :: Test.MaybeTest <- fromOk $ S.parseUnvers sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest1 pResult
        , TestCase $
             do let sResult = S.serializeUnvers Test.maybetest2
                pResult :: Test.MaybeTest <- fromOk $ S.parseUnvers sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest2 pResult
        ]

tests_String_vers
      = TestList $
        [ TestCase $ 
             do let sResult = S.serializeSmart Test.v5
                pResult :: Test.Bla <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v5 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v6
                pResult :: Test.ArrType <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v6 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v7 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v8 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.some2
                pResult :: Test.Some2 <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.some2 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.some1
                pResult :: Test.Some <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.some1 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v4
                pResult :: Test.FooBar <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v4 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v3
                pResult :: Test.FooBar <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v3 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v1
                pResult :: Test.Foo <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v1 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v2
                pResult :: Test.Foo <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v2 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.some1
                pResult :: Test.Some <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.some1 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.v5
                pResult :: Test.Bla <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.v5 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.string
                pResult :: Test.StringTest <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.string pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.string'
                pResult :: Test.StringTest2 <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.string' pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.mybool
                pResult :: Test.MyBool <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.mybool pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.bar
                pResult :: Test.Bar <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.bar pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.booltest
                pResult :: Test.BoolTest <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.booltest pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned String" Test.booltest' pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.maybetest1
                pResult :: Test.MaybeTest <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest1 pResult
        , TestCase $
             do let sResult = S.serializeSmart Test.maybetest2
                pResult :: Test.MaybeTest <- fromOk $ S.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest2 pResult
        ]
-------------------------------------------------------------------------------
-- Xml
-------------------------------------------------------------------------------

tests_Xml_vers
      = TestList $
        [ TestCase $ 
             do let sResult = X.serializeSmart Test.v5
                pResult :: Test.Bla <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v5 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.string
                pResult :: Test.StringTest <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.string pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.string'
                pResult :: Test.StringTest2 <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.string' pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v6
                pResult :: Test.ArrType <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v6 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v7 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.some2
                pResult :: Test.Some2 <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.some2 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.some1
                pResult :: Test.Some <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.some1 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.mybool
                pResult :: Test.MyBool <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.mybool pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.mybool'
                pResult :: Test.MyBool <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.mybool' pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v4
                pResult :: Test.FooBar <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v4 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v3
                pResult :: Test.FooBar <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v3 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v1
                pResult :: Test.Foo <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v1 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v2
                pResult :: Test.Foo <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v2 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.some1
                pResult :: Test.Some <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.some1 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v7 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v8 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.v6
                pResult :: Test.ArrType <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.v6 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.booltest
                pResult :: Test.BoolTest <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.booltest pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.booltest' pResult
        , TestCase $
             do let sResult = X.serializeSmart ([1,2,3,4] :: [Int])
                pResult :: [Int] <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" ([1,2,3,4] :: [Int]) pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.maybetest1
                pResult :: Test.MaybeTest <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest1 pResult
        , TestCase $
             do let sResult = X.serializeSmart Test.maybetest2
                pResult :: Test.MaybeTest <- fromOk $ X.parseSmart sResult
                assertEqual "Serializing as versioned Xml" Test.maybetest2 pResult
        ]

tests_Xml_unvers
      = TestList $
        [ TestCase $ 
             do let sResult = X.serializeUnvers Test.v5
                pResult :: Test.Bla <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v5 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.string
                pResult :: Test.StringTest <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.string pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.string'
                pResult :: Test.StringTest2 <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.string' pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v6
                pResult :: Test.ArrType <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v6 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v7 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v8 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.some2
                pResult :: Test.Some2 <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.some2 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v4
                pResult :: Test.FooBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v4 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v3
                pResult :: Test.FooBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v3 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v1
                pResult :: Test.Foo <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v1 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v2
                pResult :: Test.Foo <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v2 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.some1
                pResult :: Test.Some <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.some1 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v7
                pResult :: Test.ArrTypeBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v7 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v8 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v8
                pResult :: Test.ArrTypeFooBar <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v8 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v6
                pResult :: Test.ArrType <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v6 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.v6
                pResult :: Test.ArrType <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.v6 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.booltest
                pResult :: Test.BoolTest <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.booltest pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.booltest'
                pResult :: Test.BoolTestLong <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.booltest' pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.maybetest1
                pResult :: Test.MaybeTest <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.maybetest1 pResult
        , TestCase $
             do let sResult = X.serializeUnvers Test.maybetest2
                pResult :: Test.MaybeTest <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" Test.maybetest2 pResult
        , TestCase $
             do let sResult = X.serializeUnvers ([1,2,3,4] :: [Int])
                pResult :: [Int] <- fromOk $ X.parseUnvers sResult
                assertEqual "Serializing as unversioned Xml" ([1,2,3,4] :: [Int]) pResult
        , TestCase $
             do pResult :: Test.Easy <- fromOk $ X.parseUnvers Test.xml1
                let sResult = filter (/= ' ') $ X.serializeUnvers pResult
                assertEqual "Parsing unversioned Xml" (filter (/= ' ') Test.xml1) sResult
        , TestCase $
             do pResult :: Test.FooBar <- fromOk $ X.parseUnvers Test.xml2
                let sResult = filter (/= ' ') $ X.serializeUnvers pResult
                assertEqual "Parsing unversioned Xml" (filter (/= ' ') Test.xml2) sResult
        , TestCase $
             do pResult :: Test.Bla <- fromOk $ X.parseUnvers Test.xml3
                let sResult = filter (/= ' ') $ X.serializeUnvers pResult
                assertEqual "Parsing unversioned Xml" (filter (/= ' ') Test.xml3) sResult
        , TestCase $
             do pResult :: Test.Some <- fromOk $ X.parseUnvers Test.xml4
                let sResult = filter (/= ' ') $ X.serializeUnvers pResult
                assertEqual "Parsing unversioned Xml" (filter (/= ' ') Test.xml4) sResult
        , TestCase $
             do pResult :: Test.Some2 <- fromOk $ X.parseUnvers Test.xml5
                let sResult = filter (/= ' ') $ X.serializeUnvers pResult
                assertEqual "Parsing unversioned Xml" (filter (/= ' ') Test.xml5) sResult
        ]
-------------------------------------------------------------------------------
-- Binary (unversioned safecopy)
-------------------------------------------------------------------------------
tests_Binary
      = TestList $
        [ TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v5
                    sResult2 = B.encode Test.v5
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v6
                    sResult2 = B.encode Test.v6
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1

        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.some1
                    sResult2 = B.encode Test.some1
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.some2
                    sResult2 = B.encode Test.some2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.string
                    sResult2 = B.encode Test.string
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.string'
                    sResult2 = B.encode Test.string'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.booltest
                    sResult2 = B.encode Test.booltest
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.booltest'
                    sResult2 = B.encode Test.booltest'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.mybool
                    sResult2 = B.encode Test.mybool
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.mybool'
                    sResult2 = B.encode Test.mybool'
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v1
                    sResult2 = B.encode Test.v1
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1

        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v2
                    sResult2 = B.encode Test.v2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v4
                    sResult2 = B.encode Test.v4
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v3
                    sResult2 = B.encode Test.v3
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v2
                    sResult2 = B.encode Test.v2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v7
                    sResult2 = B.encode Test.v7
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.v8
                    sResult2 = B.encode Test.v8
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v5
                pResult :: Test.Bla <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v5 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.mybool
                pResult :: Test.MyBool <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.mybool pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.mybool'
                pResult :: Test.MyBool <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.mybool' pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.bar
                pResult :: Test.Bar <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.bar pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v7
                pResult :: Test.ArrTypeBar <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v7 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.some1
                pResult :: Test.Some <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.some1 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.some2
                pResult :: Test.Some2 <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.some2 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.string
                pResult :: Test.StringTest <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.string pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v6
                pResult :: Test.ArrType <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v6 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v8
                pResult :: Test.ArrTypeFooBar <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v8 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v4
                pResult :: Test.FooBar <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v4 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.v3
                pResult :: Test.FooBar <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.v3 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.maybetest1
                pResult :: Test.MaybeTest <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.maybetest1 pResult
        , TestCase $
             do let sResult = SMC.serializeUnvers Test.maybetest2
                pResult :: Test.MaybeTest <- either (\a -> fail $ msg a) return (SMC.parseUnvers sResult)
                assertEqual "Serializing as binary" Test.maybetest2 pResult
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.maybetest1
                    sResult2 = B.encode Test.maybetest1
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = SMC.serializeUnvers Test.maybetest2
                    sResult2 = B.encode Test.maybetest2
                assertEqual "Comparing serialized binary with Data.Serialized" sResult2 sResult1

        ]

tests_SafeCopy
      = TestList $
        [ TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v5
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v5
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart ("Bla")
                    sResult2 = prettyHex $ B.runPut $ SC.safePut "Bla"
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart ([1,2,3] :: [Int])
                    sResult2 = prettyHex $ B.runPut $ SC.safePut ([1,2,3] :: [Int])
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart (SC.Prim (42 :: Int))
                    sResult2 = prettyHex $ B.runPut $ SC.safePut (SC.Prim (42 :: Int))
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.mybool
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.mybool
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.mybool'
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.mybool'
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.bar
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.bar
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.booltest
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.booltest
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.booltest'
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.booltest'
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.string
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.string
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.string'
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.string'
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.some1
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.some1
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v1
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v1
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v2
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v2
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v3
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v3
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v4
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v4
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v6
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v6
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v7
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v7
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v8
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.v8
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.some2
                    sResult2 = prettyHex $ B.runPut $ SC.safePut Test.some2
                assertEqual "Comparing serialized binary with SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v1
                pResult :: Test.Foo <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v1 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v2
                pResult :: Test.Foo <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v2 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v3
                pResult :: Test.FooBar <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v3 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v4
                pResult :: Test.FooBar <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v4 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v5
                pResult :: Test.Bla <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v5 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.v7
                pResult :: Test.ArrTypeBar <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v7 pResult
        , TestCase $
             do let sResult = SMC.serializeSmart Test.v8
                pResult :: Test.ArrTypeFooBar <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.v8 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.some1
                pResult :: Test.Some <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.some1 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.some2
                pResult :: Test.Some2 <-
                        either (\a -> fail $ msg a) return (B.runGet SC.safeGet sResult)
                assertEqual "Parsing versioned SmartCopy binary with SafeCopy" Test.some2 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.some2
                pResult :: Test.Some2 <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.some2 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.maybetest1
                pResult :: Test.MaybeTest <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.maybetest1 pResult
        , TestCase $ 
             do let sResult = SMC.serializeSmart Test.maybetest2
                pResult :: Test.MaybeTest <-
                        either (\a -> fail $ msg a) return (SMC.parseSmart sResult)
                assertEqual "Parsing/serializing versioned binary" Test.maybetest2 pResult
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.maybetest1
                let sResult2 = prettyHex $ B.runPut $ SC.safePut Test.maybetest1
                assertEqual "Parsing/serializing versioned binary: Maybe types"
                            sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.maybetest2
                let sResult2 = prettyHex $ B.runPut $ SC.safePut Test.maybetest2
                assertEqual "Parsing/serializing versioned binary: Maybe types"
                            sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart TestV2.easy
                let sResult2 = prettyHex $ B.runPut $ SC.safePut TestV2.easy
                assertEqual "Comparing migrate type, serialized with SmartCopy and SafeCopy"
                            sResult2 sResult1
        , TestCase $
             do let sResult1 = prettyHex $ SMC.serializeSmart TestV2.some
                let sResult2 = prettyHex $ B.runPut $ SC.safePut TestV2.some
                assertEqual "Comparing migrate type, serialized with SmartCopy and SafeCopy"
                            sResult2 sResult1
        , TestCase $
             do let sResult1 = SMC.serializeSmart Test.some1
                let sResult2 = B.runPut $ SC.safePut Test.some1
                pResult1 :: TestV2.SomeV2 <-
                    either (\a -> fail $ msg a) return (SMC.parseSmart sResult1)
                pResult2 <- either (\a -> fail $ msg a) return (B.runGet SC.safeGet sResult2)
                assertEqual "Parsing migrate type with SmartCopy and SafeCopy"
                            pResult2 pResult1
        , TestCase $
             do let sResult1 = SMC.serializeSmart (Test.Easy 42)
                let sResult2 = B.runPut $ SC.safePut (Test.Easy 42)
                pResult1 <-
                    either (\a -> fail $ msg a) return (SMC.parseSmart sResult1)
                pResult2 :: TestV2.EasyV2 <- either (\a -> fail $ msg a) return (B.runGet SC.safeGet sResult2)
                assertEqual "Parsing migrate type with SmartCopy and SafeCopy"
                            pResult2 pResult1
        ]

tests_Generic
      = TestList $
        [ TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeUnvers Test.v5
                    sResult2 = prettyHex $ SMC.serializeUnvers GTest.Bla
                assertEqual "Comparing derived and manual instances: Binary" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeUnvers Test.some1
                    sResult2 = prettyHex $ SMC.serializeUnvers GTest.some1
                assertEqual "Comparing derived and manual instances: Binary" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeUnvers Test.some2
                    sResult2 = prettyHex $ SMC.serializeUnvers GTest.some2
                assertEqual "Comparing derived and manual instances: Binary" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.some1
                    sResult2 = prettyHex $ SMC.serializeSmart GTest.some1
                assertEqual "Comparing derived and manual instances: SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.some2
                    sResult2 = prettyHex $ SMC.serializeSmart GTest.some2
                assertEqual "Comparing derived and manual instances: SafeCopy" sResult2 sResult1
        , TestCase $ 
             do let sResult1 = prettyHex $ SMC.serializeSmart Test.v5
                    sResult2 = prettyHex $ SMC.serializeSmart GTest.Bla
                assertEqual "Comparing derived and manual instances: SafeCopy" sResult2 sResult1
        ]

msg a = "Failure: " ++ a


main = do args <- getArgs
          let fmtList = ["json", "string", "xml", "sc", "binary"]
          case args of
            "jsonVers":_ -> runTestTT tests_JSON_vers
            "jsonUnvers":_ -> runTestTT tests_JSON_unvers
            "stringUnvers":_ -> runTestTT tests_String_unvers
            "stringVers":_ -> runTestTT tests_String_vers
            "xmlUnvers":_ -> runTestTT tests_Xml_unvers
            "xmlVers":_ -> runTestTT tests_Xml_vers
            "binary":_ -> runTestTT tests_Binary
            "sc":_ -> runTestTT tests_SafeCopy
            "generic":_ -> runTestTT tests_Generic
            _ ->
                do runTestTT tests_JSON_unvers
                   runTestTT tests_JSON_vers
                   runTestTT tests_String_unvers
                   runTestTT tests_String_vers
                   runTestTT tests_Xml_unvers
                   runTestTT tests_Xml_vers
                   runTestTT tests_SafeCopy
                   runTestTT tests_Generic
