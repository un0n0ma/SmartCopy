{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.MonadTypesInstances (fromOk, Fail)
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

import qualified Data.ByteString as BS
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
-- General helper functions
-------------------------------------------------------------------------------

mkTestList asserts =
    do let tests = map TestCase asserts
       runTestTT $ TestList tests

mkSerParseTest dtype writeF parseF s =
    do let sResult = writeF dtype
       pres <- fromOk (parseF sResult)
       let pResult = pres `asTypeOf` dtype
       assertEqual (spTestMsg s) dtype pResult

mkParseSerTest ser parseF writeF s =
    do pResult <- fromOk (parseF ser)
       let sResult = writeF pResult
       assertEqual (psTestMsg s) ser sResult

mkCompSerTest dtype writeF1 writeF2 b s =
    do let sResult1 = (writeF1 dtype) `asTypeOf` b
           sResult2 = (writeF2 dtype) `asTypeOf` b
       assertEqual (compTestMsg s) sResult1 sResult2

compareParseAeson val a =
    do pres1 <- fromOk $ J.parseUnvers val
       let Json.Success pres2 = Json.fromJSON val
           pResult1 = pres1 `asTypeOf` a
           pResult2 = pres2 `asTypeOf` a
       assertEqual "Comparing parsed with Aeson" pResult2 pResult1

spTestMsg s = "Serializing/parsing: " ++ s ++ "."
psTestMsg s = "Parsing/serializing: " ++ s ++ "."
compTestMsg s = "Comparing SmartCopy-serialized datatypes with " ++ s ++ "."

-------------------------------------------------------------------------------
-- JSON
-------------------------------------------------------------------------------

tests_JSON_vers
    = do let s = "JSON"
         mkTestList 
             [ mkSerParseTest Test.v1 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v2 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v3 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v4 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v5 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v6a J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v6b J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v7 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.v8 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.some1 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.some2 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.booltest J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.booltest' J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.mybool J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.mybool' J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.maybetest1 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.maybetest2 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.string J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.string' J.serializeSmart J.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) J.serializeSmart J.parseSmart s
             , mkSerParseTest (42 :: Int) J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.easy J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.easy J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.some J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.some J.serializeSmart J.parseSmart s
             , mkParseSerTest Test.js1 (J.parseSmart :: Json.Value -> Fail Test.MyDouble) 
                                       J.serializeSmart s
             ]
                        

tests_JSON_unvers
    = do let s = "JSON"
             s2 = "Aeson"
         mkTestList
             [ mkSerParseTest Test.v1 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v2 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v3 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v4 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v5 J.serializeUnvers J.parseUnvers s
 
             , mkSerParseTest Test.v6a J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v6b J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v7 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.v8 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.some1 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.some2 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.booltest J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.booltest' J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.mybool J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.mybool' J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.maybetest1 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.maybetest2 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.string J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.string' J.serializeUnvers J.parseUnvers s
             , mkSerParseTest ([1,2,3,4] :: [Int]) J.serializeUnvers J.parseUnvers s 
             , mkSerParseTest (42 :: Int) J.serializeUnvers J.parseUnvers s
             , mkSerParseTest TestV2.easy J.serializeUnvers J.parseUnvers s
             , mkSerParseTest TestV2.some J.serializeUnvers J.parseUnvers s
             , mkParseSerTest Test.js2 (J.parseUnvers :: Json.Value -> Fail Test.MyDouble)
                                      J.serializeUnvers s
             , mkParseSerTest Test.js3 (J.parseUnvers :: Json.Value -> Fail Test.FooBar)
                                      J.serializeUnvers s
             , mkCompSerTest Test.some2 J.serializeUnvers Json.toJSON 
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.some1 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v4 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v3 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v1 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v2 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v8 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.string J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.booltest J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.booltest' J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v6a J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v6b J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.v7 J.serializeUnvers Json.toJSON 
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.string' J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.bar J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , compareParseAeson Test.js2 (undefined :: Test.MyDouble)
             , compareParseAeson Test.js3 (undefined :: Test.FooBar)
             ]

-------------------------------------------------------------------------------
-- String-Encoding
-------------------------------------------------------------------------------

tests_String_unvers
    = do let s = "String"
         mkTestList
             [ mkSerParseTest Test.v1 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v2 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v3 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v4 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v5 S.serializeUnvers S.parseUnvers s
 
             , mkSerParseTest Test.v6a S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v6b S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v7 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.v8 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.some1 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.some2 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.booltest S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.booltest' S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.mybool S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.mybool' S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.maybetest1 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.maybetest2 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.string S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.string' S.serializeUnvers S.parseUnvers s
             , mkSerParseTest ([1,2,3,4] :: [Int]) S.serializeUnvers S.parseUnvers s 
             , mkSerParseTest (42 :: Int) S.serializeUnvers S.parseUnvers s
             , mkSerParseTest TestV2.easy S.serializeUnvers S.parseUnvers s
             , mkSerParseTest TestV2.some S.serializeUnvers S.parseUnvers s
             , mkParseSerTest Test.s1 (S.parseUnvers :: String -> Fail Test.Easy)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s2 (S.parseUnvers :: String -> Fail Test.FooBar)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s3 (S.parseUnvers :: String -> Fail Test.FooBar)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s4 (S.parseUnvers :: String -> Fail Test.ArrType)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s5 (S.parseUnvers :: String -> Fail Test.ArrTypeBar)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s6 (S.parseUnvers :: String -> Fail Test.ArrTypeBar)
                                      S.serializeUnvers s
             , mkParseSerTest Test.s7 (S.parseUnvers :: String -> Fail Test.ArrTypeFooBar)
                                      S.serializeUnvers s
             ]

tests_String_vers
    = do let s = "String-versioned"
         mkTestList
             [ mkSerParseTest Test.v1 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v2 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v3 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v4 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v5 S.serializeSmart S.parseSmart s
 
             , mkSerParseTest Test.v6a S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v6b S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v7 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.v8 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.some1 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.some2 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.booltest S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.booltest' S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.mybool S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.mybool' S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.maybetest1 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.maybetest2 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.string S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.string' S.serializeSmart S.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) S.serializeSmart S.parseSmart s 
             , mkSerParseTest (42 :: Int) S.serializeSmart S.parseSmart s
             , mkSerParseTest TestV2.easy S.serializeSmart S.parseSmart s
             , mkSerParseTest TestV2.some S.serializeSmart S.parseSmart s
             ]

-------------------------------------------------------------------------------
-- Xml
-------------------------------------------------------------------------------

tests_Xml_vers
    = do let s = "XML-versioned"
         mkTestList
             [ mkSerParseTest Test.v1 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v2 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v3 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v4 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v5 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v6a X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v6b X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v7 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.v8 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.some1 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.some2 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.booltest X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.booltest' X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.mybool X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.mybool' X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.maybetest1 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.maybetest2 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.string X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.string' X.serializeSmart X.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) X.serializeSmart X.parseSmart s 
             , mkSerParseTest (42 :: Int) X.serializeSmart X.parseSmart s
             , mkSerParseTest ("String") X.serializeSmart X.parseSmart s
             , mkSerParseTest True X.serializeSmart X.parseSmart s
             , mkSerParseTest TestV2.easy X.serializeSmart X.parseSmart s
             , mkSerParseTest TestV2.some X.serializeSmart X.parseSmart s
             ]

tests_Xml_unvers
    = do let s = "XML-unversioned"
         mkTestList
             [ mkSerParseTest Test.v1 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v2 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v3 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v4 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v5 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v6a X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v6b X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v7 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.v8 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.some1 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.some2 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.booltest X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.booltest' X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.mybool X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.mybool' X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.maybetest1 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.maybetest2 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.string X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.string' X.serializeUnvers X.parseUnvers s
             , mkSerParseTest ([1,2,3,4] :: [Int]) X.serializeUnvers X.parseUnvers s 
             , mkSerParseTest (42 :: Int) X.serializeUnvers X.parseUnvers s
             , mkSerParseTest ("String") X.serializeUnvers X.parseUnvers s
             , mkSerParseTest True X.serializeUnvers X.parseUnvers s
             , mkSerParseTest TestV2.easy X.serializeUnvers X.parseUnvers s
             , mkSerParseTest TestV2.some X.serializeUnvers X.parseUnvers s
             , mkParseSerTest Test.xml1 (X.parseUnvers :: String -> Fail Test.Easy)
                                      X.serializeUnvers s
             , mkParseSerTest Test.xml2 (X.parseUnvers :: String -> Fail Test.FooBar)
                                      X.serializeUnvers s
             , mkParseSerTest Test.xml3 (X.parseUnvers :: String -> Fail Test.Bla)
                                      X.serializeUnvers s
             , mkParseSerTest Test.xml4 (X.parseUnvers :: String -> Fail Test.Some)
                                      X.serializeUnvers s
             , mkParseSerTest Test.xml5 (X.parseUnvers :: String -> Fail Test.Some2)
                                      X.serializeUnvers s
             ]
-------------------------------------------------------------------------------
-- Binary (unversioned safecopy)
-------------------------------------------------------------------------------
tests_Binary
    = do let s = "Binary-unversioned"
             s2 = "Data.Serialize ByteStrings"
         mkTestList
             [ mkSerParseTest Test.v1 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v2 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v3 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v4 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v5 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v6a SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v6b SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v7 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.v8 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.some1 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.some2 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.booltest SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.booltest' SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.mybool SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.mybool' SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.maybetest1 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.maybetest2 SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.string SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest Test.string' SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest ([1,2,3,4] :: [Int]) SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest (42 :: Int) SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest TestV2.easy SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkSerParseTest TestV2.some SMC.serializeUnvers
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseUnvers ser)) s
             , mkCompSerTest Test.v5 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v6a SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v6b SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v7 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v8 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v1 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v2 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v3 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.v4 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.some1 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.some2 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.string SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.string' SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.booltest SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.booltest' SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.mybool' SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.mybool SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.maybetest1 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.maybetest2 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             ]

tests_SafeCopy
    = do let s = "versioned Binary"
             s2 = "SafeCopy ByteStrings"
         mkTestList
             [ mkCompSerTest Test.maybetest2 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v1 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v2 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v3 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v4 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v5 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v6a (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v6b (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v7 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.v8 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest ([1,2,3] :: [Int]) (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest ("Bla") (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest (42 :: Int) (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.bar (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.mybool (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.mybool' (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.booltest (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.booltest' (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.string (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.string' (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkSerParseTest Test.v1 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v2 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v3 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v4 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v5 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v6a SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v6b SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v7 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.v8 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.some1 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.some2 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.booltest SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.booltest' SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.mybool SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.mybool' SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.maybetest1 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.maybetest2 SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.string SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest Test.string' SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest ([1,2,3,4] :: [Int]) SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest (42 :: Int) SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest "String" SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest TestV2.easy SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             , mkSerParseTest TestV2.some SMC.serializeSmart
                    (\ser -> either (\a -> fail $ msg a) return (SMC.parseSmart ser)) s
             ]

tests_Generic
    = do let s = "GHC-Generic instances"
         mkTestList
             [ mkCompSerTest Test.v5 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.Bla) (undefined :: String) s
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.some2) (undefined :: String) s
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some2) (undefined :: String) s
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v5 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.Bla) (undefined :: String) s
             , mkCompSerTest Test.v1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.mybool (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.mybool' (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.mybool') (undefined :: String) s
             ]

msg a = "Failure: " ++ a


main = do args <- getArgs
          let fmtList = ["json", "string", "xml", "sc", "binary"]
          case args of
            "jsonVers":_ -> tests_JSON_vers
            "jsonUnvers":_ -> tests_JSON_unvers
            "stringUnvers":_ -> tests_String_unvers
            "stringVers":_ -> tests_String_vers
            "xmlUnvers":_ -> tests_Xml_unvers
            "xmlVers":_ -> tests_Xml_vers
            "binary":_ -> tests_Binary
            "sc":_ -> tests_SafeCopy
            "generic":_ -> tests_Generic
            _ ->
                do tests_JSON_unvers
                   tests_JSON_vers
                   tests_String_unvers
                   tests_String_vers
                   tests_Xml_unvers
                   tests_Xml_vers
                   tests_Binary
                   tests_Generic
                   tests_SafeCopy
