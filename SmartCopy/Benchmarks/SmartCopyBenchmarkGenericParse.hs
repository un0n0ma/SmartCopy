{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesDerived as GTest

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.SmartCopy.Formats.SafeCopy as SMC

import Criterion.Main
import Criterion.Monad
import Criterion.Types

main = defaultMain
           [ bgroup "Parse versioned ADTs"
             [ bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.Some2)
               (SMC.serializeSmart GTest.some2))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Some2)
               (SMC.serializeSmart Test.some2))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.Some)
               (SMC.serializeSmart GTest.some1))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Some)
               (SMC.serializeSmart Test.some1))
             ]
           , bgroup "Parse versioned empty-constructor types"
             [ bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.Bar)
               (SMC.serializeSmart GTest.bar))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Bar)
               (SMC.serializeSmart Test.bar))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.Bla)
               (SMC.serializeSmart GTest.Bla))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Bla)
               (SMC.serializeSmart Test.Bla))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.MyBool)
               (SMC.serializeSmart GTest.mybool))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.MyBool)
               (SMC.serializeSmart Test.mybool))
             ]
           , bgroup "Parse versioned Array types"
             [ bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.ArrType)
               (SMC.serializeSmart GTest.v6a))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrType)
               (SMC.serializeSmart Test.v6a))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.ArrTypeBar)
               (SMC.serializeSmart GTest.v7))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrTypeBar)
               (SMC.serializeSmart Test.v7))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.ArrTypeFooBar)
               (SMC.serializeSmart GTest.v8))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrTypeFooBar)
               (SMC.serializeSmart Test.v8))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.StringTest)
               (SMC.serializeSmart GTest.string))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.StringTest)
               (SMC.serializeSmart Test.string))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.StringTest2)
               (SMC.serializeSmart GTest.string'))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.StringTest2)
               (SMC.serializeSmart Test.string'))
             , bench "SmartCopy Generic"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String GTest.BoolTestLong)
               (SMC.serializeSmart GTest.booltest'))
             , bench "SmartCopy, manually defined"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.BoolTestLong)
               (SMC.serializeSmart Test.booltest'))
             ]
           ]
