module Tests.TestOutput where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesDerived as GTest
import qualified Tests.TestInstancesMigrate as TestV2

import Tests.Tests (mkIDs)

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.SmartCopy
import Hexdump
import System.Environment

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.SmartCopy.Formats.JSON as J 
               ( serializeSmart
               , serializeUnvers
               , serializeLastKnown
               , parseLastKnown
               , parseLastKnown
               , parseSmart
               , parseUnvers
               , encodeUnvers
               , encodeSmart
               , decodeSmart
               , decodeUnvers
               )
import qualified Data.SmartCopy.Formats.String as S
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               , serializeLastKnown
               , parseLastKnown
               )
import qualified Data.SmartCopy.Formats.XmlLike as X
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               , fromXmlString
               , toXmlString
               , serializeLastKnown
               , parseLastKnown
               )
import qualified Data.SmartCopy.Formats.SafeCopy as SMC
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               )

main = do args <- getArgs
          let fmtList = ["json", "string", "xml", "safecopy"]
          case args of
            "json":_ ->
                do putStrLn "DATATYPES AS JSON VALUES:"
                   print (J.serializeSmart Test.v3)
                   print (J.serializeSmart Test.v4)
                   print (J.serializeSmart Test.v8)
                   print (J.serializeSmart Test.some1)
                   print (J.serializeSmart Test.some2)
                   print (J.serializeSmart Test.booltest')
                   print (J.serializeSmart TestV2.easy)
                   print (J.serializeSmart Test.v6a)
                   print (J.serializeSmart Test.v6b)
                   print (J.serializeSmart Test.v1)
                   print (J.serializeSmart Test.v2)
                   print (J.serializeSmart Test.v5)
                   print (J.serializeSmart Test.v7)
                   print (J.serializeSmart Test.string)
                   print (J.serializeSmart Test.string')
                   print (J.serializeSmart Test.booltest)
                   print (J.serializeSmart Test.booltest')
                   print (J.serializeSmart Test.bar)
                   print (J.serializeSmart Test.maybetest1)
                   print (J.serializeSmart Test.maybetest2)
                   print (J.serializeUnvers Test.v1)
                   print (J.serializeUnvers Test.v2)
                   print (J.serializeUnvers Test.v3)
                   print (J.serializeUnvers Test.v4)
                   print (J.serializeUnvers Test.v5)
                   print (J.serializeUnvers Test.v6a)
                   print (J.serializeUnvers Test.v6b)
                   print (J.serializeUnvers Test.v7)
                   print (J.serializeUnvers Test.v8)
                   print (J.serializeUnvers Test.some2)
                   print (J.serializeUnvers Test.some1)
                   print (J.serializeUnvers Test.string)
                   print (J.serializeUnvers Test.string')
                   print (J.serializeUnvers Test.booltest)
                   print (J.serializeUnvers Test.booltest')
                   print (J.serializeUnvers Test.bar)
                   print (J.serializeUnvers Test.maybetest1)
                   print (J.serializeUnvers Test.maybetest2)
                   putStrLn "SERIALIZING JSON WITH DERIVED INSTANCES:"
                   print (J.serializeUnvers GTest.Bla)
                   print (J.serializeUnvers GTest.some1)
                   print (J.serializeUnvers GTest.some2)
                   putStrLn "PARSING JSON VALUES:"
                   print (J.parseSmart Test.js1 :: Fail Test.Easy)
                   print (J.parseUnvers Test.js2 :: Fail Test.MyDouble)
                   print (J.parseUnvers Test.js3 :: Fail Test.FooBar)
                   print (J.parseSmart (J.serializeSmart Test.string') :: Fail Test.StringTest2)
                   putStrLn "ENCODING AND DECODING:"
                   BSL.putStrLn (J.encodeSmart Test.some1)
                   BSL.putStrLn (J.encodeSmart Test.v3)
                   BSL.putStrLn (J.encodeSmart Test.v4)
                   BSL.putStrLn (J.encodeUnvers Test.v4)
                   BSL.putStrLn (J.encodeUnvers Test.v3)
                   BSL.putStrLn (J.encodeUnvers Test.v5)
                   BSL.putStrLn (J.encodeUnvers Test.v6a)
                   BSL.putStrLn (J.encodeUnvers Test.v6b)
                   BSL.putStrLn (J.encodeUnvers Test.v1)
                   BSL.putStrLn (J.encodeUnvers Test.v2)
                   BSL.putStrLn (J.encodeUnvers Test.v7)
                   BSL.putStrLn (J.encodeUnvers Test.v8)
                   BSL.putStrLn (J.encodeSmart Test.v8)
                   print (J.decodeSmart $ J.encodeSmart Test.some1 :: Fail Test.Some)
                   print (J.decodeSmart $ J.encodeSmart Test.v3 :: Fail Test.FooBar)
                   print (J.decodeSmart $ J.encodeSmart Test.v4 :: Fail Test.FooBar)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v4 :: Fail Test.FooBar)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v3 :: Fail Test.FooBar)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v5 :: Fail Test.Bla)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v6a :: Fail Test.ArrType)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v6b :: Fail Test.ArrType)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v1 :: Fail Test.Foo)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v2 :: Fail Test.Foo)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v7 :: Fail Test.ArrTypeBar)
                   print (J.decodeUnvers $ J.encodeUnvers Test.v8 :: Fail Test.ArrTypeFooBar)
                   print (J.decodeSmart $ J.encodeSmart Test.v8 :: Fail Test.ArrTypeFooBar)

                   putStrLn "BACK-MIGRATION:"
                   print (J.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV1"])
                   print (J.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV2", "SpamV1"])
                   print (J.serializeLastKnown Test.some1 $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   print (J.serializeLastKnown GTest.some1 $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   print (J.serializeLastKnown TestV2.some $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   print (J.serializeLastKnown TestV2.some $ mkIDs ["SomeV1", "SomeV2", "SpamV2", "SpamV1"])
                   print (J.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1"])
                   print (J.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2"])
                   print (J.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2", "EasyV3"])
                   print (J.parseLastKnown (J.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)
                   print (J.parseLastKnown (J.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)
            "string":_ ->
                do putStrLn "DATATYPES AS STRING VALUES:"
                   putStrLn (S.serializeUnvers Test.maybetest1)
                   putStrLn (S.serializeUnvers Test.maybetest2)
                   putStrLn (S.serializeUnvers Test.maybeX)
                   putStrLn (S.serializeUnvers Test.v3)
                   putStrLn (S.serializeUnvers Test.v2)
                   putStrLn (S.serializeSmart Test.v1)
                   putStrLn (S.serializeSmart Test.v5)
                   putStrLn (S.serializeSmart Test.v6a)
                   putStrLn (S.serializeSmart Test.v6b)
                   putStrLn (S.serializeSmart Test.v8)
                   putStrLn (S.serializeSmart Test.string)
                   putStrLn (S.serializeSmart Test.some1)
                   putStrLn (S.serializeSmart Test.some2)
                   putStrLn (S.serializeSmart Test.booltest)
                   putStrLn (S.serializeSmart Test.booltest')
                   putStrLn (S.serializeSmart Test.string')
                   putStrLn (S.serializeSmart Test.s6parsed)
                   putStrLn "PARSING STRING VALUES:"
                   print (S.parseSmart (S.serializeSmart TestV2.someNewSpam) :: Fail TestV2.Some)
                   print (S.parseUnvers Test.somestring1 :: Fail Test.Some)
                   print (S.parseUnvers Test.somestring2 :: Fail Test.Some2)
                   print (S.parseUnvers Test.s1 :: Fail Test.Easy)
                   print (S.parseUnvers Test.s2 :: Fail Test.FooBar)
                   print (S.parseUnvers Test.s3 :: Fail Test.FooBar)
                   print (S.parseUnvers Test.s4 :: Fail Test.ArrType)
                   print (S.parseUnvers Test.s5 :: Fail Test.ArrTypeBar)
                   print (S.parseUnvers Test.s6 :: Fail Test.ArrTypeBar)
                   print (S.parseUnvers Test.s7 :: Fail Test.ArrTypeFooBar)
                   putStrLn "BACK-MIGRATION:"
                   putStrLn (S.serializeLastKnown TestV2.someNewSpam' $
                       mkIDs ["SomeV1", "SomeV2", "SpamV1", "SomeV3"])
                   putStrLn (S.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV1"])
                   putStrLn (S.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV2", "SpamV1"])
                   putStrLn (S.serializeLastKnown Test.some1 $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   putStrLn (S.serializeLastKnown GTest.some1 $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   putStrLn (S.serializeLastKnown TestV2.some $ mkIDs ["SomeV1", "SpamV2", "SpamV1"])
                   putStrLn (S.serializeLastKnown TestV2.some $ mkIDs ["SomeV1", "SomeV2", "SpamV2", "SpamV1"])
                   putStrLn (S.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1"])
                   putStrLn (S.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2"])
                   putStrLn (S.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2", "EasyV3"])
                   print (S.parseLastKnown (S.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)
                   print (S.parseLastKnown (S.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)

            "xml":_ ->
                do putStrLn "DATATYPES XML-ENCODED:"
                   putStrLn (X.toXmlString $ X.serializeUnvers Test.v6a)
                   putStrLn (X.toXmlString $ X.serializeUnvers Test.v6b)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v6a)
                   putStrLn (X.toXmlString $ X.serializeSmart (42 :: Int))
                   putStrLn (X.toXmlString $ X.serializeUnvers (42 :: Int))
                   putStrLn (X.toXmlString $ X.serializeSmart ([1,2,3,4] :: [Int]))
                   putStrLn (X.toXmlString $ X.serializeUnvers ([1,2,3,4] :: [Int]))
                   putStrLn (X.toXmlString $ X.serializeSmart Test.some1)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v2)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v1)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v3)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.some2)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v4)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v5)
                   putStrLn (X.toXmlString $ X.serializeUnvers Test.v6a)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v6a)
                   putStrLn (X.toXmlString $ X.serializeUnvers Test.v6b)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v6b)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v7)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.v8)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.string)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.booltest)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.booltest')
                   putStrLn (X.toXmlString $ X.serializeSmart Test.s6parsed)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.maybetest1)
                   putStrLn (X.toXmlString $ X.serializeSmart Test.maybetest2)
                   putStrLn "PARSING XML:"
                   print (X.parseUnvers $ X.fromXmlString Test.xml1 :: Fail Test.Easy)
                   print (X.parseUnvers $ X.fromXmlString Test.xml2 :: Fail Test.FooBar)
                   print (X.parseUnvers $ X.fromXmlString Test.xml3 :: Fail Test.Bla)
                   print (X.parseUnvers $ X.fromXmlString Test.xml4 :: Fail Test.Some)
                   print (X.parseUnvers $ X.fromXmlString Test.xml5 :: Fail Test.Some2)
                   putStrLn "BACK-MIGRATION:"
                   print (X.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV1"])
                   print (X.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV1", "SomeV2", "SomeV3", "SpamV2", "SpamV1"])
                   putStrLn $ X.toXmlString $ X.serializeLastKnown Test.some1 $
                       mkIDs ["SomeV1", "SpamV2", "SpamV1"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown GTest.some1 $
                       mkIDs ["SomeV1", "SpamV2", "SpamV1"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown TestV2.some $
                       mkIDs ["SomeV1", "SpamV2", "SpamV1"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown TestV2.some $
                       mkIDs ["SomeV1", "SomeV2", "SpamV2", "SpamV1"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2"]
                   putStrLn $ X.toXmlString $ X.serializeLastKnown TestV2.easy' $
                       mkIDs ["EasyV1", "EasyV2", "EasyV3"]
                   print (X.parseLastKnown (X.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)
                   print (X.parseLastKnown (X.serializeLastKnown TestV2.someNewSpam $
                       mkIDs ["SomeV3", "SpamV1", "SpamV2"]) :: Fail TestV2.Some)
            "safecopy":_ ->
                do putStrLn "DATATYPES SAFECOPY-ENCODED:"
                   print $ prettyHex $ SMC.serializeSmart Test.mybool
                   print $ prettyHex $ SMC.serializeSmart Test.mybool'
                   print $ prettyHex $ SMC.serializeSmart Test.bar
                   print $ prettyHex $ SMC.serializeSmart Test.v1
                   print $ prettyHex $ SMC.serializeSmart Test.v2
                   print $ prettyHex $ SMC.serializeSmart Test.v3
                   print $ prettyHex $ SMC.serializeSmart Test.v3'
                   print $ prettyHex $ SMC.serializeSmart (42.0 :: Double)
                   print $ prettyHex $ SMC.serializeSmart Test.v4
                   print $ prettyHex $ SMC.serializeSmart (23 :: Int)
                   print $ prettyHex $ SMC.serializeSmart Test.v5
                   print $ prettyHex $ SMC.serializeSmart Test.v6a
                   print $ prettyHex $ SMC.serializeSmart Test.v6b
                   print $ prettyHex $ SMC.serializeSmart Test.v7
                   print $ prettyHex $ SMC.serializeSmart Test.v8
                   print $ prettyHex $ SMC.serializeSmart Test.string'
                   print $ prettyHex $ SMC.serializeSmart Test.booltest'
                   print $ prettyHex $ SMC.serializeSmart TestV2.easy
                   print $ prettyHex $ SMC.serializeSmart TestV2.easy'
                   print $ prettyHex $ SMC.serializeSmart TestV2.someOld
                   print $ prettyHex $ SMC.serializeSmart Test.some1
                   print $ prettyHex $ B.runPut $ SC.safePut TestV2.someOld
                   print $ prettyHex $ B.runPut $ SC.safePut Test.some1
                   print (B.runGet SC.safeGet (B.runPut $ SC.safePut Test.some1) :: Either String Test.Some)
                   print (B.runGet SC.safeGet (B.runPut $ SC.safePut TestV2.someOld) :: Either String Test.Some)
            _ ->
                putStrLn $ "Please choose output mode: " ++ show fmtList
