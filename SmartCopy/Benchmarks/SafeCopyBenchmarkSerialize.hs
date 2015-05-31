{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesMigrate as TestV2

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.SmartCopy.Formats.SafeCopy as SMC

import Criterion.Main
import Criterion.Monad
import Criterion.Types

main = defaultMain
           [ bgroup "Serialize ADTs: Some (Spam Int Int)"
             [ bench "SmartCopy" (whnf SMC.serializeSmart Test.some2)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.some2)
             ]
           , bgroup "Serialize ADTs: Some (Spam Int) Int"
             [ bench "SmartCopy" (whnf SMC.serializeSmart Test.some1)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.some1)
             ]
           , bgroup "Serialize empty-constructor types"
             [ bench "SmartCopy" (whnf SMC.serializeSmart Test.bar)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.bar)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.Bla)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.Bla)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.mybool)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.mybool)
             ]
           , bgroup "Serialize primitives"
             [ bench "SmartCopy" (whnf SMC.serializeSmart (42 :: Int))
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) (42 :: Int))
             , bench "SmartCopy" (whnf SMC.serializeSmart ("Benchmark" :: String))
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) ("Benchmark" :: String))
             , bench "SmartCopy" (whnf SMC.serializeSmart ([1,2,3,4] :: [Int]))
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) ([1,2,3,4] :: [Int]))
             ]
           , bgroup "Serialize Array types"
             [ bench "SmartCopy" (whnf SMC.serializeSmart Test.v6a)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.v6a)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.v7)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.v7)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.v8)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.v8)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.string)
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.string)
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.string')
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.string')
             , bench "SmartCopy" (whnf SMC.serializeSmart Test.booltest')
             , bench "SafeCopy" (whnf (B.runPut . SC.safePut) Test.booltest')
             ]
           ]
