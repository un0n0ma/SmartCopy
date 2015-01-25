{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Aeson

--data Foo = Foo { bar1 :: Bar, bar2 :: Bar } deriving Generic
--data Bar = Bar Int String deriving Generic
data Bar = BarLeft | BarRight Foo deriving (Show, Generic)
data PatientV1
     = PatientV1
     { pat_id :: Int
     , pat_name :: String
     , pat_diagnosis :: [String]
     , pat_number :: String
     } deriving Generic

data Foo = Foo Int Bar deriving (Show, Generic)

data SumTest a = Sum1 Int a | Sum2 [SumTest a] | Sum3 deriving Generic

data Person = Person String Int deriving Generic

instance ToJSON Foo
instance ToJSON Bar
instance ToJSON PatientV1
instance ToJSON a => ToJSON (SumTest a)
instance ToJSON Person

--v1 = Foo (Bar 42 "bar1") (Bar 24 "bar2")
v1 :: Foo
v1 = Foo 2 BarLeft
--v2 = PatientV1 1234 "Olaf Fischer" ["F22.0", "K50.1"] "0761-1234"
v3 = Sum2 [Sum1 1 (2 :: Int), Sum1 3 (1 :: Int), Sum2 [], Sum3]
v4 = Person "Person" 1
v2 :: Foo
v2 = Foo 42 (BarRight (Foo 41 BarLeft))


main = do --print $ toJSON v1
          print $ toJSON v3
          print $ toJSON v4
          print $ toJSON v1
          print $ toJSON v2
