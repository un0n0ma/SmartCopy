module TestOutput where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified SmartCopy.Formats.JSON as J 
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               , encode
               )
import qualified SmartCopy.Formats.StringFormat as S
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               )
import qualified SmartCopy.Formats.XmlLikeFormat as X
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               )
import qualified SmartCopy.Formats.SafeCopy as SMC
               ( serializeSmart
               , serializeUnvers
               , parseSmart
               , parseUnvers
               )
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesDerived as GTest

import SmartCopy.MonadTypesInstances
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Serialize as B

import Hexdump
import System.Environment


main = do args <- getArgs
          let fmtList = ["json", "string", "xml", "safecopy"]
          case args of
            "json":_ ->
                do putStrLn "DATATYPES AS JSON VALUES:"
                   print (J.serializeSmart Test.v1)
                   print (J.serializeSmart Test.v2)
                   print (J.serializeSmart Test.v3)
                   print (J.serializeSmart Test.v4)
                   print (J.serializeSmart Test.v5)
                   print (J.serializeSmart Test.v6)
                   print (J.serializeSmart Test.v7)
                   print (J.serializeSmart Test.v8)
                   print (J.serializeSmart Test.some2)
                   print (J.serializeSmart Test.some1)
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
                   print (J.serializeUnvers Test.v6)
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
                   putStrLn "ENCODING:"
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v4))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v3))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v5))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v6))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v1))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v2))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v7))
                   BSL.putStrLn (J.encode (J.serializeSmart Test.v8))
            "string":_ ->
                do putStrLn "DATATYPES AS STRING VALUES:"
                   print (S.serializeUnvers Test.maybetest1)
                   print (S.serializeUnvers Test.maybetest2)
                   print (S.serializeUnvers Test.v3)
                   print (S.serializeUnvers Test.v2)
                   print (S.serializeSmart Test.v1)
                   print (S.serializeSmart Test.v5)
                   print (S.serializeSmart Test.v8)
                   print (S.serializeSmart Test.string)
                   print (S.serializeSmart Test.some1)
                   print (S.serializeSmart Test.some2)
                   print (S.serializeSmart Test.booltest)
                   print (S.serializeSmart Test.booltest')
                   print (S.serializeSmart Test.string')
                   print (S.serializeSmart Test.s6parsed)
                   putStrLn "PARSING STRING VALUES:"
                   print (S.parseUnvers Test.somestring1 :: Fail Test.Some)
                   print (S.parseUnvers Test.somestring2 :: Fail Test.Some2)
                   print (S.parseUnvers Test.s1 :: Fail Test.Easy)
                   print (S.parseUnvers Test.s2 :: Fail Test.FooBar)
                   print (S.parseUnvers Test.s3 :: Fail Test.FooBar)
                   print (S.parseUnvers Test.s4 :: Fail Test.ArrType)
                   print (S.parseUnvers Test.s5 :: Fail Test.ArrTypeBar)
                   print (S.parseUnvers Test.s6 :: Fail Test.ArrTypeBar)
                   print (S.parseUnvers Test.s7 :: Fail Test.ArrTypeFooBar)
            "xml":_ ->
                do putStrLn "DATATYPES XML-ENCODED:"
                   putStrLn (X.serializeSmart ([1,2,3,4] :: [Int]))
                   putStrLn (X.serializeUnvers ([1,2,3,4] :: [Int]))
                   putStrLn (X.serializeSmart Test.some1)
                   putStrLn (X.serializeSmart Test.v2)
                   putStrLn (X.serializeSmart Test.v1)
                   putStrLn (X.serializeSmart Test.v3)
                   putStrLn (X.serializeSmart Test.some2)
                   putStrLn (X.serializeSmart Test.v4)
                   putStrLn (X.serializeSmart Test.v5)
                   putStrLn (X.serializeSmart Test.v6)
                   putStrLn (X.serializeSmart Test.v7)
                   putStrLn (X.serializeSmart Test.v8)
                   putStrLn (X.serializeSmart Test.string)
                   putStrLn (X.serializeSmart Test.booltest)
                   putStrLn (X.serializeSmart Test.booltest')
                   putStrLn (X.serializeSmart Test.s6parsed)
                   putStrLn (X.serializeSmart Test.maybetest1)
                   putStrLn (X.serializeSmart Test.maybetest2)
                   putStrLn "PARSING XML:"
                   print (X.parseUnvers Test.xml1 :: Fail Test.Easy)
                   print (X.parseUnvers Test.xml2 :: Fail Test.FooBar)
                   print (X.parseUnvers Test.xml3 :: Fail Test.Bla)
                   print (X.parseUnvers Test.xml4 :: Fail Test.Some)
                   print (X.parseUnvers Test.xml5 :: Fail Test.Some2)
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
                   print $ prettyHex $ SMC.serializeSmart Test.v6
                   print $ prettyHex $ SMC.serializeSmart Test.v7
                   print $ prettyHex $ SMC.serializeSmart Test.v8
                   print $ prettyHex $ SMC.serializeSmart Test.string'
                   print $ prettyHex $ SMC.serializeSmart Test.booltest'
            _ ->
                putStrLn $ "Specify a format out of " ++ show fmtList


