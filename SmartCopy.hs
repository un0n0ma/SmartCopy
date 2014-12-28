{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module SmartCopy where

import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Identity
import Data.Monoid


-------------------------------------------------------------------------------
-- Datatypes for testing
-------------------------------------------------------------------------------

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
 
instance ProcessXml Foo
instance ProcessXml Bar
instance ProcessXml PatientV1
instance ProcessXml a => ProcessXml (SumTest a)

v1 = Foo (Bar 42 "bar1") (Bar 24 "bar2")
v2 = PatientV1 1234 "Olaf Fischer" ["F22.0", "K50.1"] "0761-1234"
v3 = Sum2 [Sum1 1 (2 :: Int), Sum1 3 (1 :: Int), Sum2 [], Sum3]

-------------------------------------------------------------------------------
-- GENERICS
-------------------------------------------------------------------------------

encode = encode' 0

data PrimValue = PrimInt Int
               | PrimString String
               | PrimChar Char
               deriving (Show, Eq)

class ProcessXml a where
      encode' :: Int -> a -> IO () --- Int for numbered fields
      default encode' :: (Generic a, GProcessXml (Rep a))
                => Int -> a -> IO ()
      encode' i x = gencode i (from x)

class GProcessXml t where
      gencode :: Int -> t x -> IO ()

instance ProcessXml PrimValue where
    encode' _ i = writeValue i
 
instance ProcessXml Int where
    encode' _ i = writeValue (PrimInt i)

instance ProcessXml Char where
    encode' _ c = writeValue (PrimChar c)

instance ProcessXml String where
    encode' _ s = writeValue (PrimString (show s))

instance ProcessXml a => ProcessXml [a] where
    encode' _ [] = return ()
    encode' i (x:xs) = encode' i x >> encode' i xs

-----------------------------
-- Gen Instances
-----------------------------

instance GProcessXml U1 where
    gencode _ _ = return ()

instance (GProcessXml a, Constructor c) => GProcessXml (C1 c a) where
    gencode i m1 = do enterDataCon (conName m1)
                      gencode 0 (unM1 m1)
                      leaveDataCon (conName m1)

instance (GProcessXml a, Datatype d) => GProcessXml (D1 d a) where
    gencode i m1 = gencode 0 (unM1 m1)

instance (GProcessXml a, Selector s) => GProcessXml (S1 s a) where
    gencode i m1 = do case selName m1 of
                        "" -> gencode i (unM1 m1)
                        _ -> 
                            do enterField i (Just $ selName m1)
                               gencode i (unM1 m1)
                               leaveField i (Just $ selName m1)

instance (GProcessXml a, GProcessXml b) => GProcessXml (a :+: b) where
    gencode i (L1 a) = gencode i a
    gencode i (R1 a) = gencode i a

instance (GProcessXml a, GProcessXml b) => GProcessXml (a :*: b) where
    gencode i (a :*: b) = gencode i a >> gencode (i+1) b

instance ProcessXml a => GProcessXml (K1 g a) where
    gencode i (K1 a) = encode' i a

-------------------------------------------------------------------------------
--Monads
-------------------------------------------------------------------------------

newtype Contain m = Contain { getBuild :: m } deriving Show

instance Monoid (Contain String) where
    mempty = Contain []
    mappend ma mb = Contain (getBuild ma ++ getBuild mb)

class Monad m => Format m where
    enterDataCon :: String -> m ()
    enterField :: Int -> Maybe String -> m ()
    writeValue :: PrimValue -> m ()
    leaveField :: Int -> Maybe String -> m ()
    leaveDataCon :: String -> m ()

instance Format IO where
    enterDataCon = \s -> 
            do let xs = "<" `mappend` s `mappend` ">"
               putStrLn xs
    enterField = \num ms ->
        do let s = case ms of
                     Just a -> a
                     Nothing -> (show num)
               xs = "<" `mappend` s `mappend` ">"
           putStrLn xs
    writeValue = \s ->
        case s of
          PrimInt i -> putStrLn (show i)
          PrimString s -> putStrLn s
    leaveField = \num ms ->
        do let s = case ms of
                     Just a -> a
                     Nothing -> (show num)
               xs = "<\\" `mappend` (show s) `mappend` ">"
           putStrLn xs
    leaveDataCon = \s ->
        do let xs = "<\\" `mappend` s `mappend` ">"
           putStrLn xs

newtype XmlMonad m = XmlMonad { getXml :: Contain m } deriving Show

runXml :: XmlMonad String -> IO ()
runXml xml = putStrLn $ getBuild $ getXml xml

main = do encode v1 
          encode v2
          encode v3
