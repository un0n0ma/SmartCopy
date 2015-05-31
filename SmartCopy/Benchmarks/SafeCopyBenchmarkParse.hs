{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test

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
           [ bgroup "Parse ADTs"
             [ bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Some2)
               (SMC.serializeSmart Test.some2))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.Some2)
               (B.runPut $ SC.safePut Test.some2))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Some)
               (SMC.serializeSmart Test.some1))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.Some)
               (B.runPut $ SC.safePut Test.some1))
             ]
           , bgroup "Parse empty-constructor types"
             [ bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Bar)
               (SMC.serializeSmart Test.bar))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.Bar)
               (B.runPut $ SC.safePut Test.bar))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.Bla)
               (SMC.serializeSmart Test.Bla))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.Bla)
               (B.runPut $ SC.safePut Test.Bla))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.MyBool)
               (SMC.serializeSmart Test.mybool))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.MyBool)
               (B.runPut $ SC.safePut Test.mybool))
             ]
           , bgroup "Parse primitives"
             [ bench "SmartCopy" (whnf (SMC.parseSmart :: BS.ByteString -> Either String Int)
               (SMC.serializeSmart (42 :: Int)))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Int)
               (B.runPut $ SC.safePut (42 :: Int)))
             , bench "SmartCopy" (whnf (SMC.parseSmart :: BS.ByteString -> Either String String)
               (SMC.serializeSmart ("Benchmark" :: String)))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String String)
               (B.runPut $ SC.safePut ("Benchmark" :: String)))
             , bench "SmartCopy" (whnf (SMC.parseSmart :: BS.ByteString -> Either String [Int])
               (SMC.serializeSmart ([1,2,3,4] :: [Int])))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String [Int])
               (B.runPut $ SC.safePut ([1,2,3,4] :: [Int])))
             ]
           , bgroup "Parse Array types"
             [ bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrType)
               (SMC.serializeSmart Test.v6a))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.ArrType)
               (B.runPut $ SC.safePut Test.v6a))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrTypeBar)
               (SMC.serializeSmart Test.v7))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.ArrTypeBar)
               (B.runPut $ SC.safePut Test.v7))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.ArrTypeFooBar)
               (SMC.serializeSmart Test.v8))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.ArrTypeFooBar)
               (B.runPut $ SC.safePut Test.v8))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.StringTest)
               (SMC.serializeSmart Test.string))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.StringTest)
               (B.runPut $ SC.safePut Test.string))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.StringTest2)
               (SMC.serializeSmart Test.string'))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.StringTest2)
               (B.runPut $ SC.safePut Test.string'))
             , bench "SmartCopy"
               (whnf (SMC.parseSmart :: BS.ByteString -> Either String Test.BoolTestLong)
               (SMC.serializeSmart Test.booltest'))
             , bench "SafeCopy"
               (whnf (B.runGet SC.safeGet :: BS.ByteString -> Either String Test.BoolTestLong)
               (B.runPut $ SC.safePut Test.booltest'))
             ]
           ]
