{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tests where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified TestInstances as Test
import qualified TestInstancesDerived as GTest
import qualified TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Hashable
import Data.Scientific
import Data.SmartCopy
import System.Environment
import Test.HUnit
import Test.QuickCheck
import Hexdump

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap as M
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.SmartCopy.Formats.JSON as J
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified Data.SmartCopy.Formats.SafeCopy as SMC
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified Data.SmartCopy.Formats.String as S
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
import qualified Data.SmartCopy.Formats.XmlLike as X
                 ( serializeUnvers
                 , parseUnvers
                 , serializeSmart
                 , parseSmart
                 )
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
    do let sResult1 = writeF1 dtype `asTypeOf` b
           sResult2 = writeF2 dtype `asTypeOf` b
       assertEqual (compTestMsg s) sResult1 sResult2

mkGenericParseTest val1 val2 parseF =
    do pres1 <- fromOk $ parseF val1
       assertEqual "Comparing parsing results of manual vs. derived instances" val2 pres1

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

testsJSONVers
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
             , mkSerParseTest Test.maybeX J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.sumtest1 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.sumtest2 J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.string J.serializeSmart J.parseSmart s
             , mkSerParseTest Test.string' J.serializeSmart J.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) J.serializeSmart J.parseSmart s
             , mkSerParseTest ([] :: [String]) J.serializeSmart J.parseSmart s
             , mkSerParseTest (42 :: Int) J.serializeSmart J.parseSmart s
             , mkSerParseTest (BSC.pack "ByteString") J.serializeSmart J.parseSmart s
             , mkSerParseTest (T.pack "Text") J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.easy J.serializeSmart J.parseSmart s
             , mkSerParseTest TestV2.some J.serializeSmart J.parseSmart s
             , mkParseSerTest Test.js1 (J.parseSmart :: Json.Value -> Fail Test.MyDouble) 
                                       J.serializeSmart s
             ]
                        

testsJSONUnvers
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
             , mkSerParseTest Test.sumtest1 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.sumtest2 J.serializeUnvers J.parseUnvers s
             , mkSerParseTest Test.maybeX J.serializeUnvers J.parseUnvers s
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
             , mkCompSerTest Test.sumtest1 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.sumtest2 J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , mkCompSerTest Test.maybeX J.serializeUnvers Json.toJSON
                                       (undefined :: Json.Value) s2
             , compareParseAeson Test.js2 (undefined :: Test.MyDouble)
             , compareParseAeson Test.js3 (undefined :: Test.FooBar)
             ]

-------------------------------------------------------------------------------
-- String-Encoding
-------------------------------------------------------------------------------

testsStringUnvers
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
             , mkSerParseTest Test.maybeX S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.sumtest1 S.serializeUnvers S.parseUnvers s
             , mkSerParseTest Test.sumtest2 S.serializeUnvers S.parseUnvers s
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

testsStringVers
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
             , mkSerParseTest Test.maybeX S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.sumtest1 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.sumtest2 S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.string S.serializeSmart S.parseSmart s
             , mkSerParseTest Test.string' S.serializeSmart S.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) S.serializeSmart S.parseSmart s 
             , mkSerParseTest (42 :: Int) S.serializeSmart S.parseSmart s
             , mkSerParseTest ([] :: [String]) S.serializeSmart S.parseSmart s
             , mkSerParseTest TestV2.easy S.serializeSmart S.parseSmart s
             , mkSerParseTest TestV2.some S.serializeSmart S.parseSmart s
             ]

-------------------------------------------------------------------------------
-- Xml
-------------------------------------------------------------------------------

testsXmlVers
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
             , mkSerParseTest Test.maybeX X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.sumtest1 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.sumtest2 X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.string X.serializeSmart X.parseSmart s
             , mkSerParseTest Test.string' X.serializeSmart X.parseSmart s
             , mkSerParseTest ([1,2,3,4] :: [Int]) X.serializeSmart X.parseSmart s 
             , mkSerParseTest (42 :: Int) X.serializeSmart X.parseSmart s
             , mkSerParseTest ([] :: [String]) X.serializeSmart X.parseSmart s
             , mkSerParseTest "String" X.serializeSmart X.parseSmart s
             , mkSerParseTest (T.pack "Text") X.serializeSmart X.parseSmart s
             , mkSerParseTest (BSC.pack "ByteString") X.serializeSmart X.parseSmart s
             , mkSerParseTest True X.serializeSmart X.parseSmart s
             , mkSerParseTest TestV2.easy X.serializeSmart X.parseSmart s
             , mkSerParseTest TestV2.some X.serializeSmart X.parseSmart s
             ]

testsXmlUnvers
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
             , mkSerParseTest Test.maybeX X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.sumtest1 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.sumtest2 X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.string X.serializeUnvers X.parseUnvers s
             , mkSerParseTest Test.string' X.serializeUnvers X.parseUnvers s
             , mkSerParseTest ([1,2,3,4] :: [Int]) X.serializeUnvers X.parseUnvers s 
             , mkSerParseTest (42 :: Int) X.serializeUnvers X.parseUnvers s
             , mkSerParseTest "String" X.serializeUnvers X.parseUnvers s
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
testsBinary
    = do let s = "Binary-unversioned"
             s2 = "Data.Serialize ByteStrings"
         mkTestList
             [ mkSerParseTest Test.v1 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v3 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v4 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v5 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v6a SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v6b SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v7 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.v8 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.some1 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.some2 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.booltest SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.booltest' SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.mybool SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.mybool' SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.maybetest1 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.maybetest2 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.maybeX SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.sumtest1 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.sumtest2 SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.string SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest Test.string' SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest ([1,2,3,4] :: [Int]) SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest (42 :: Int) SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest TestV2.easy SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
             , mkSerParseTest TestV2.some SMC.serializeUnvers
                    (either (fail . msg) return . SMC.parseUnvers) s
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
             , mkCompSerTest Test.maybeX SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.sumtest1 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             , mkCompSerTest Test.sumtest2 SMC.serializeUnvers B.encode (undefined :: BS.ByteString) s2
             ]

testsSafeCopy
    = do let s = "versioned Binary"
             s2 = "SafeCopy ByteStrings"
         mkTestList
             [ mkCompSerTest Test.maybetest2 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.maybeX (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.sumtest1 (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest Test.sumtest2 (prettyHex . SMC.serializeSmart)
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
             , mkCompSerTest "Bla" (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest (42 :: Int) (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest (T.pack "Text") (prettyHex . SMC.serializeSmart)
                   (prettyHex . B.runPut . SC.safePut) (undefined :: String) s2
             , mkCompSerTest (BSC.pack "ByteString") (prettyHex . SMC.serializeSmart)
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
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v2 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v3 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v4 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v5 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v6a SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v6b SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v7 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.v8 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.some1 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.some2 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.booltest SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.booltest' SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.mybool SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.mybool' SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.maybetest1 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.maybetest2 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.maybeX SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.sumtest1 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.sumtest2 SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.string SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest Test.string' SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest ([1,2,3,4] :: [Int]) SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest (42 :: Int) SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest "String" SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest (BSC.pack "ByteString") SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest (T.pack "Text") SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest TestV2.easy SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             , mkSerParseTest TestV2.some SMC.serializeSmart
                    (either (fail . msg) return . SMC.parseSmart) s
             ]

testsGenericSerVersioned
    = do let s = "GHC-Generic instances: Serialize versioned"
         mkTestList
             [ mkCompSerTest Test.v5 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.Bla) (undefined :: String) s
             , mkCompSerTest Test.spam (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.spam) (undefined :: String) s
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some2) (undefined :: String) s
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some2) (undefined :: String) s
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v5 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.Bla) (undefined :: String) s
             , mkCompSerTest Test.v1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.mybool (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.mybool' (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.v3 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.v7 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.sumtest1 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.sumtest2) (undefined :: String) s
             , mkCompSerTest Test.booltest (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.booltest' (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.booltest') (undefined :: String) s
             , mkCompSerTest Test.string (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' (prettyHex . SMC.serializeSmart)
                   (\_ -> prettyHex $ SMC.serializeSmart GTest.string') (undefined :: String) s
             , mkCompSerTest Test.some2 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.some2) (undefined :: Json.Value) s
             , mkCompSerTest Test.some1 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.some1) (undefined :: Json.Value) s
             , mkCompSerTest Test.bar J.serializeSmart
                   (\_ -> J.serializeSmart GTest.bar) (undefined :: Json.Value) s
             , mkCompSerTest Test.mybool' J.serializeSmart
                   (\_ -> J.serializeSmart GTest.mybool') (undefined :: Json.Value) s
             , mkCompSerTest Test.mybool J.serializeSmart
                   (\_ -> J.serializeSmart GTest.mybool) (undefined :: Json.Value) s
             , mkCompSerTest Test.v1 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v1) (undefined :: Json.Value) s
             , mkCompSerTest Test.v2 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v2) (undefined :: Json.Value) s
             , mkCompSerTest Test.v3 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v3) (undefined :: Json.Value) s
             , mkCompSerTest Test.v7 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v7) (undefined :: Json.Value) s
             , mkCompSerTest Test.v4 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v4) (undefined :: Json.Value) s
             , mkCompSerTest Test.v8 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.v8) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybetest1 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.maybetest1) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybetest2 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.maybetest2) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybeX J.serializeSmart
                   (\_ -> J.serializeSmart GTest.maybeX) (undefined :: Json.Value) s
             , mkCompSerTest Test.booltest J.serializeSmart
                   (\_ -> J.serializeSmart GTest.booltest) (undefined :: Json.Value) s
             , mkCompSerTest Test.booltest' J.serializeSmart
                   (\_ -> J.serializeSmart GTest.booltest') (undefined :: Json.Value) s
             , mkCompSerTest Test.string J.serializeSmart
                   (\_ -> J.serializeSmart GTest.string) (undefined :: Json.Value) s
             , mkCompSerTest Test.string' J.serializeSmart
                   (\_ -> J.serializeSmart GTest.string') (undefined :: Json.Value) s
             , mkCompSerTest Test.sumtest1 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.sumtest1) (undefined :: Json.Value) s
             , mkCompSerTest Test.sumtest2 J.serializeSmart
                   (\_ -> J.serializeSmart GTest.sumtest2) (undefined :: Json.Value) s
             , mkCompSerTest Test.v1 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.v3 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.bar X.serializeSmart
                   (\_ -> X.serializeSmart GTest.bar) (undefined :: String) s
             , mkCompSerTest Test.mybool X.serializeSmart
                   (\_ -> X.serializeSmart GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.mybool' X.serializeSmart
                   (\_ -> X.serializeSmart GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.some1 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v7 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX X.serializeSmart
                   (\_ -> X.serializeSmart GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.booltest X.serializeSmart
                   (\_ -> X.serializeSmart GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.booltest' X.serializeSmart
                   (\_ -> X.serializeSmart GTest.booltest') (undefined :: String) s
             , mkCompSerTest Test.string X.serializeSmart
                   (\_ -> X.serializeSmart GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' X.serializeSmart
                   (\_ -> X.serializeSmart GTest.string') (undefined :: String) s
             , mkCompSerTest Test.sumtest1 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 X.serializeSmart
                   (\_ -> X.serializeSmart GTest.sumtest2) (undefined :: String) s
             , mkCompSerTest Test.mybool' S.serializeSmart
                   (\_ -> S.serializeSmart GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.mybool S.serializeSmart
                   (\_ -> S.serializeSmart GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.v1 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.v3 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.some1 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v7 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX S.serializeSmart
                   (\_ -> S.serializeSmart GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.booltest S.serializeSmart
                   (\_ -> S.serializeSmart GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.booltest' S.serializeSmart
                   (\_ -> S.serializeSmart GTest.booltest') (undefined :: String) s
             , mkCompSerTest Test.string S.serializeSmart
                   (\_ -> S.serializeSmart GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' S.serializeSmart
                   (\_ -> S.serializeSmart GTest.string') (undefined :: String) s
             , mkCompSerTest Test.sumtest1 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 S.serializeSmart
                   (\_ -> S.serializeSmart GTest.sumtest2) (undefined :: String) s
             ]

testsGenericSer
    = do let s = "GHC-Generic instances: Serialize"
         mkTestList
             [ mkCompSerTest Test.v5 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.Bla) (undefined :: String) s
             , mkCompSerTest Test.some1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.some2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.some2) (undefined :: String) s
             , mkCompSerTest Test.v1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.mybool (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.mybool' (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.v3 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.v3 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v7 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.sumtest1 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.sumtest2) (undefined :: String) s
             , mkCompSerTest Test.booltest (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.booltest' (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.booltest') (undefined :: String) s
             , mkCompSerTest Test.string (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' (prettyHex . SMC.serializeUnvers)
                   (\_ -> prettyHex $ SMC.serializeUnvers GTest.string') (undefined :: String) s
             , mkCompSerTest Test.some2 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.some2) (undefined :: Json.Value) s
             , mkCompSerTest Test.some1 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.some1) (undefined :: Json.Value) s
             , mkCompSerTest Test.bar J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.bar) (undefined :: Json.Value) s
             , mkCompSerTest Test.mybool' J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.mybool') (undefined :: Json.Value) s
             , mkCompSerTest Test.mybool J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.mybool) (undefined :: Json.Value) s
             , mkCompSerTest Test.v1 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v1) (undefined :: Json.Value) s
             , mkCompSerTest Test.v2 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v2) (undefined :: Json.Value) s
             , mkCompSerTest Test.v3 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v3) (undefined :: Json.Value) s
             , mkCompSerTest Test.v7 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v7) (undefined :: Json.Value) s
             , mkCompSerTest Test.v4 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v4) (undefined :: Json.Value) s
             , mkCompSerTest Test.v8 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.v8) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybetest1 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.maybetest1) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybetest2 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.maybetest2) (undefined :: Json.Value) s
             , mkCompSerTest Test.maybeX J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.maybeX) (undefined :: Json.Value) s
             , mkCompSerTest Test.sumtest1 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.sumtest1) (undefined :: Json.Value) s
             , mkCompSerTest Test.sumtest2 J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.sumtest2) (undefined :: Json.Value) s
             , mkCompSerTest Test.booltest J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.booltest) (undefined :: Json.Value) s
             , mkCompSerTest Test.booltest' J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.booltest') (undefined :: Json.Value) s
             , mkCompSerTest Test.string J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.string) (undefined :: Json.Value) s
             , mkCompSerTest Test.string' J.serializeUnvers
                   (\_ -> J.serializeUnvers GTest.string') (undefined :: Json.Value) s
             , mkCompSerTest Test.v1 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.v3 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.bar X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.bar) (undefined :: String) s
             , mkCompSerTest Test.mybool X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.mybool' X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.some1 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v7 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.booltest X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.booltest' X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.booltest') (undefined :: String) s
             , mkCompSerTest Test.string X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.string') (undefined :: String) s
             , mkCompSerTest Test.sumtest1 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 X.serializeUnvers
                   (\_ -> X.serializeUnvers GTest.sumtest2) (undefined :: String) s
             , mkCompSerTest Test.mybool' S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.mybool') (undefined :: String) s
             , mkCompSerTest Test.mybool S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.mybool) (undefined :: String) s
             , mkCompSerTest Test.v1 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v1) (undefined :: String) s
             , mkCompSerTest Test.v2 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v2) (undefined :: String) s
             , mkCompSerTest Test.v3 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v3) (undefined :: String) s
             , mkCompSerTest Test.v4 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v4) (undefined :: String) s
             , mkCompSerTest Test.some1 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.some1) (undefined :: String) s
             , mkCompSerTest Test.v7 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v7) (undefined :: String) s
             , mkCompSerTest Test.v8 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.v8) (undefined :: String) s
             , mkCompSerTest Test.maybetest1 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.maybetest1) (undefined :: String) s
             , mkCompSerTest Test.maybetest2 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.maybetest2) (undefined :: String) s
             , mkCompSerTest Test.maybeX S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.maybeX) (undefined :: String) s
             , mkCompSerTest Test.booltest S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.booltest) (undefined :: String) s
             , mkCompSerTest Test.string S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.string) (undefined :: String) s
             , mkCompSerTest Test.string' S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.string') (undefined :: String) s
             , mkCompSerTest Test.sumtest1 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.sumtest1) (undefined :: String) s
             , mkCompSerTest Test.sumtest2 S.serializeUnvers
                   (\_ -> S.serializeUnvers GTest.sumtest2) (undefined :: String) s
             ]

testsGenericParse
    = do let s = "GHC-Generic instances: Parse"
         mkTestList
             [ mkGenericParseTest (SMC.serializeUnvers Test.v1) 
               GTest.v1 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.v2)
               GTest.v2 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.v3)
               GTest.v3 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.v4)
               GTest.v4 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.v8)
               GTest.v8 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.maybetest1)
               GTest.maybetest1 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.maybetest2)
               GTest.maybetest2 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.maybeX)
               GTest.maybeX (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.sumtest1)
               GTest.sumtest1 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.sumtest2)
               GTest.sumtest2 (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.mybool)
               GTest.mybool (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.booltest)
               GTest.booltest (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.booltest')
               GTest.booltest' (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.string)
               GTest.string (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (SMC.serializeUnvers Test.string')
               GTest.string' (either (fail . msg) return . SMC.parseUnvers)
             , mkGenericParseTest (J.serializeUnvers Test.v1)
               GTest.v1 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.v2)
               GTest.v2 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.v3)
               GTest.v3 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.v4)
               GTest.v4 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.v8)
               GTest.v8 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.mybool)
               GTest.mybool J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.BarLeft)
               GTest.BarLeft J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.mybool')
               GTest.mybool' J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.maybetest1)
               GTest.maybetest1 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.maybeX)
               GTest.maybeX J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.maybetest2)
               GTest.maybetest2 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.booltest)
               GTest.booltest J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.booltest')
               GTest.booltest' J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.string)
               GTest.string J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.string')
               GTest.string' J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.sumtest1)
               GTest.sumtest1 J.parseUnvers
             , mkGenericParseTest (J.serializeUnvers Test.sumtest2)
               GTest.sumtest2 J.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.maybetest1)
               GTest.maybetest1 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.maybeX)
               GTest.maybeX X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.booltest)
               GTest.booltest X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.booltest')
               GTest.booltest' X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.maybetest2)
               GTest.maybetest2 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.string)
               GTest.string X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.string')
               GTest.string' X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.sumtest2)
               GTest.sumtest2 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.sumtest1)
               GTest.sumtest1 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.v1)
               GTest.v1 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.v2)
               GTest.v2 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.v3)
               GTest.v3 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.v4)
               GTest.v4 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.v8)
               GTest.v8 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.mybool)
               GTest.mybool X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.maybetest1)
               GTest.maybetest1 X.parseUnvers
             , mkGenericParseTest (X.serializeUnvers Test.maybetest2)
               GTest.maybetest2 X.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.v1)
               GTest.v1 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.v2)
               GTest.v2 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.v3)
               GTest.v3 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.v4)
               GTest.v4 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.v8)
               GTest.v8 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.mybool)
               GTest.mybool S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.maybetest1)
               GTest.maybetest1 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.maybetest2)
               GTest.maybetest2 S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.string)
               GTest.string S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.string')
               GTest.string' S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.booltest)
               GTest.booltest S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.booltest')
               GTest.booltest' S.parseUnvers
             , mkGenericParseTest (S.serializeUnvers Test.maybeX)
               GTest.maybeX S.parseUnvers
             ]


testsGenericParseVersioned
    = do let s = "GHC-Generic instances: Parse versioned"
         mkTestList
             [ mkGenericParseTest (SMC.serializeSmart Test.v1) 
               GTest.v1 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.v2)
               GTest.v2 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.v3)
               GTest.v3 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.v4)
               GTest.v4 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.some1)
               GTest.some1 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.some2)
               GTest.some2 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.spam)
               GTest.spam (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.v8)
               GTest.v8 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.maybetest1)
               GTest.maybetest1 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.maybetest2)
               GTest.maybetest2 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.maybeX)
               GTest.maybeX (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.mybool)
               GTest.mybool (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.booltest)
               GTest.booltest (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.booltest')
               GTest.booltest' (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.string)
               GTest.string (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.string')
               GTest.string' (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.sumtest1)
               GTest.sumtest1 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (SMC.serializeSmart Test.sumtest2)
               GTest.sumtest2 (either (fail . msg) return . SMC.parseSmart)
             , mkGenericParseTest (J.serializeSmart Test.v1)
               GTest.v1 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.v2)
               GTest.v2 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.v3)
               GTest.v3 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.v4)
               GTest.v4 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.v8)
               GTest.v8 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.mybool)
               GTest.mybool J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.BarLeft)
               GTest.BarLeft J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.mybool')
               GTest.mybool' J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.maybetest1)
               GTest.maybetest1 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.maybeX)
               GTest.maybeX J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.maybetest2)
               GTest.maybetest2 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.booltest)
               GTest.booltest J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.booltest')
               GTest.booltest' J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.string)
               GTest.string J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.string')
               GTest.string' J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.some1)
               GTest.some1 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.some2)
               GTest.some2 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.spam)
               GTest.spam J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.sumtest1)
               GTest.sumtest1 J.parseSmart
             , mkGenericParseTest (J.serializeSmart Test.sumtest2)
               GTest.sumtest2 J.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.maybetest1)
               GTest.maybetest1 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.maybeX)
               GTest.maybeX X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.booltest)
               GTest.booltest X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.booltest')
               GTest.booltest' X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.maybetest2)
               GTest.maybetest2 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.string)
               GTest.string X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.string')
               GTest.string' X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.v1)
               GTest.v1 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.v2)
               GTest.v2 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.v3)
               GTest.v3 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.v4)
               GTest.v4 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.v8)
               GTest.v8 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.mybool)
               GTest.mybool X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.maybetest1)
               GTest.maybetest1 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.maybetest2)
               GTest.maybetest2 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.some1)
               GTest.some1 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.some2)
               GTest.some2 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.spam)
               GTest.spam X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.spam)
               GTest.spam X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.sumtest1)
               GTest.sumtest1 X.parseSmart
             , mkGenericParseTest (X.serializeSmart Test.sumtest2)
               GTest.sumtest2 X.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.v1)
               GTest.v1 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.v2)
               GTest.v2 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.v3)
               GTest.v3 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.v4)
               GTest.v4 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.v8)
               GTest.v8 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.mybool)
               GTest.mybool S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.maybetest1)
               GTest.maybetest1 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.maybetest2)
               GTest.maybetest2 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.string)
               GTest.string S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.string')
               GTest.string' S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.booltest)
               GTest.booltest S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.booltest')
               GTest.booltest' S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.maybeX)
               GTest.maybeX S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.some1)
               GTest.some1 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.some2)
               GTest.some2 S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.spam)
               GTest.spam S.parseSmart
             , mkGenericParseTest (S.serializeSmart Test.sumtest1)
               GTest.sumtest1 S.parseSmart
             ]

msg a = "Failure: " ++ a


main = do args <- getArgs
          case args of
            "jsonVers":_ -> testsJSONVers
            "jsonUnvers":_ -> testsJSONUnvers
            "stringUnvers":_ -> testsStringUnvers
            "stringVers":_ -> testsStringVers
            "xmlUnvers":_ -> testsXmlUnvers
            "xmlVers":_ -> testsXmlVers
            "binary":_ -> testsBinary
            "sc":_ -> testsSafeCopy
            "genericS":_ -> testsGenericSer
            "genericP":_ -> testsGenericParse
            "genericSVers":_ -> testsGenericSerVersioned
            "genericPVers":_ -> testsGenericParseVersioned
            _ ->
                do testsJSONUnvers
                   testsJSONVers
                   testsStringUnvers
                   testsStringVers
                   testsXmlUnvers
                   testsXmlVers
                   testsBinary
                   testsSafeCopy
                   testsGenericSer
                   testsGenericParse
                   testsGenericSerVersioned
                   testsGenericParseVersioned
