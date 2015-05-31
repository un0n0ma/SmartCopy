{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as Test
import qualified Tests.TestInstancesDerived as GTest

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
           [ bgroup "Serialize versioned ADTs: Some (Spam Int Int)"
             [ bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.some2)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.some2)
             ]
           , bgroup "Serialize versioned ADTs: Some (Spam Int) Int"
             [ bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.some1)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.some1)
             ]
           , bgroup "Serialize versioned empty-constructor types"
             [ bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.bar)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.bar)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.Bla)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.Bla)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.mybool)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.mybool)
             ]
           , bgroup "Serialize versioned Array types"
             [ bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.v6a)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.v6a)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.v7)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.v7)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.v8)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.v8)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.string)
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.string)
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.string')
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.string')
             , bench "SmartCopy Generic" (whnf SMC.serializeSmart GTest.booltest')
             , bench "SmartCopy, manually defined" (whnf SMC.serializeSmart Test.booltest')
             ]
           ]
