{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson
import JSONTypes
import SmartCopy

data Foo = Foo { bar1 :: Bar, bar2 :: Bar } deriving Generic
data Bar = Bar Int String deriving Generic
data PatientV1
     = PatientV1
     { pat_id :: Int
     , pat_name :: String
     , pat_diagnosis :: [String]
     , pat_number :: String
     } deriving Generic

data SumTest a = Sum1 Int a | Sum2 [SumTest a] | Sum3 deriving Generic

instance SmartCopy Foo
instance SmartCopy Bar
instance SmartCopy PatientV1
instance SmartCopy a => SmartCopy (SumTest a)

v1 = Foo (Bar 42 "bar1") (Bar 24 "bar2")
v2 = PatientV1 1234 "Olaf Fischer" ["F22.0", "K50.1"] "0761-1234"
v3 = Sum2 [Sum1 1 (2 :: Int), Sum1 3 (1 :: Int), Sum2 [], Sum3]

runJSON :: SmartCopy a => a -> IO ()
runJSON a = print $ unPack (serialize a :: JSON Value)

main = do runJSON v1
          runJSON v2
          runJSON v3
