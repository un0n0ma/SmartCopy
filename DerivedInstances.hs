{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative
import GHC.Generics
import Data.Aeson hiding (parseJSON)
import JSON
import SmartCopy
import Data.Data

data Foo = Foo { bar1 :: Bar, bar2 :: Bar } deriving (Generic, Show, Typeable)
data EmptyType = EmptyType deriving (Generic, Show, Typeable)
data Bar = Bar Int String deriving (Generic, Show, Typeable)
data PatientV1
     = PatientV1
     { pat_id :: Int
     , pat_name :: String
     , pat_diagnosis :: [String]
     , pat_number :: String
     } deriving (Generic, Show, Typeable)

data SumTest a = Sum1 Int a | Sum2 [SumTest a] | Sum3 deriving (Generic, Show, Typeable)

instance SmartCopy EmptyType JSON
instance Data EmptyType
instance SmartCopy Foo JSON
instance SmartCopy Bar JSON
instance SmartCopy PatientV1 JSON
instance (Data a, Typeable a, SmartCopy a JSON) => SmartCopy (SumTest a) JSON
instance Data Foo
instance Data Bar
instance Data PatientV1
instance Data a => Data (SumTest a)

v1 = Foo (Bar 42 "bar1") (Bar 24 "bar2")
v2 = PatientV1 1234 "Olaf Fischer" ["F22.0", "K50.1"] "0761-1234"
v3 = Sum2 [Sum1 1 (2 :: Int), Sum1 3 (1 :: Int), Sum2 [], Sum3]
v4 = EmptyType

main = do runJSONEncode v1
          runJSONEncode v2
          runJSONEncode v3
          runJSONEncode v4
          print $ unPack (runJSONParse (serialize v1 :: JSON Value) :: JSON Foo)
