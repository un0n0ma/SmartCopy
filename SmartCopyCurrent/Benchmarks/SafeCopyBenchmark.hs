{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified SmartCopy.Formats.SafeCopy as SMC
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC

import Criterion.Main
import Criterion.Monad
import Criterion.Types

main = defaultMain
           [ bgroup "serialize nested: Some (Spam Int Int)"
             [ bench "smartC" (whnf SMC.serializeSmart Test.some2)
             , bench "safeC" (whnf SC.safePut Test.some2)
             ]
           , bgroup "serialize nested: Some (Spam Int) Int"
             [ bench "smartC" (whnf SMC.serializeSmart Test.some1)
             , bench "safeC" (whnf SC.safePut Test.some1)
             ]
           , bgroup "serialize No-Fields types"
             [ bench "smartC" (whnf SMC.serializeSmart Test.bar)
             , bench "smartC" (whnf SMC.serializeSmart Test.Bla)
             , bench "smartC" (whnf SMC.serializeSmart Test.mybool)
             , bench "safeC" (whnf SC.safePut Test.bar)
             , bench "safeC" (whnf SC.safePut Test.Bla)
             , bench "safeC" (whnf SC.safePut Test.mybool)
             ]
           , bgroup "serialize primitives"
             [ bench "smartC" (whnf SMC.serializeSmart (42 :: Int))
             , bench "smartC" (whnf SMC.serializeSmart ("Benchmark" :: String))
             , bench "smartC" (whnf SMC.serializeSmart ([1,2,3,4] :: [Int]))
             , bench "safeC" (whnf SC.safePut (42 :: Int))
             , bench "safeC" (whnf SC.safePut ("Benchmark" :: String))
             , bench "safeC" (whnf SC.safePut ([1,2,3,4] :: [Int]))
             ]
           , bgroup "serialize Array types"
             [ bench "smartC" (whnf SMC.serializeSmart Test.v6a)
             , bench "smartC" (whnf SMC.serializeSmart Test.v7)
             , bench "smartC" (whnf SMC.serializeSmart Test.v8)
             , bench "smartC" (whnf SMC.serializeSmart Test.string)
             , bench "smartC" (whnf SMC.serializeSmart Test.string')
             , bench "smartC" (whnf SMC.serializeSmart Test.booltest')
             , bench "safeC" (whnf SC.safePut Test.v6a)
             , bench "safeC" (whnf SC.safePut Test.v7)
             , bench "safeC" (whnf SC.safePut Test.v8)
             , bench "safeC" (whnf SC.safePut Test.string)
             , bench "safeC" (whnf SC.safePut Test.string')
             , bench "safeC" (whnf SC.safePut Test.booltest')
             ]
           ]
