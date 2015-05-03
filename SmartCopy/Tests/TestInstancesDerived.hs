{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.TestInstancesDerived where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.SmartCopy

import qualified Data.SafeCopy as SC
-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Data.Typeable
import GHC.Generics

data Bla = Bla deriving (Show, Eq, Generic)
data Spam2 = Spam2 Int Int deriving (Show, Eq, Generic)
data Spam = Spam Int deriving (Show, Eq, Generic)
data Some2 = Some2 Spam2 deriving (Show, Eq, Generic)
data Some = Some Spam Int deriving (Show, Eq, Generic)
data Bar = BarLeft | BarRight Foo deriving (Show, Eq, Generic)
data Foo = Foo Int Bar deriving (Eq, Show, Generic)
data MyBool = MyFalse | MyTrue deriving (Eq, Show, Generic)
data FooBar = Foo0 MyBool MyDouble | Bar0 { value :: Int, foobar :: FooBar }
    deriving (Eq, Show, Generic)
data MyDouble = MyDouble Double deriving (Eq, Show, Generic)
data ArrType = ArrType [Int] deriving (Eq, Show, Generic)
data ArrTypeBar = ArrTypeBar [Bar] deriving (Eq, Show, Generic)
data ArrTypeFooBar = ArrTypeFooBar [FooBar] deriving (Eq, Show, Generic)
data MaybeTest = MaybeTest Int (Maybe Bar) deriving (Eq, Show, Generic)
data MaybeTestX = MaybeTestX [Maybe Int] Bar [String] deriving (Eq, Show, Generic)
data BoolTest = BoolTest Bool deriving (Eq, Show, Generic)
data BoolTestLong = BoolTestLong { blist :: [Bool], b :: Bool, slist :: [String] }
    deriving (Eq, Show, Generic)
data StringTest = StringTest String deriving (Eq, Show, Generic)
data StringTest2 = StringTest2 String [Int] deriving (Eq, Show, Generic)
data SumTest = SumTest1 | SumTest2 Int | SumTest3 Int Int deriving (Eq, Show, Generic)

SC.deriveSafeCopy 1 'SC.base ''StringTest
SC.deriveSafeCopy 1 'SC.base ''FooBar
SC.deriveSafeCopy 1 'SC.base ''Bar
SC.deriveSafeCopy 1 'SC.base ''Foo
SC.deriveSafeCopy 1 'SC.base ''MyDouble
SC.deriveSafeCopy 1 'SC.base ''MyBool
SC.deriveSafeCopy 1 'SC.base ''StringTest2
SC.deriveSafeCopy 1 'SC.base ''BoolTestLong
SC.deriveSafeCopy 1 'SC.base ''ArrType
SC.deriveSafeCopy 1 'SC.base ''Bla
SC.deriveSafeCopy 1 'SC.base ''Some
SC.deriveSafeCopy 1 'SC.base ''Some2
SC.deriveSafeCopy 1 'SC.base ''Spam
SC.deriveSafeCopy 1 'SC.base ''Spam2
SC.deriveSafeCopy 1 'SC.base ''ArrTypeBar
SC.deriveSafeCopy 1 'SC.base ''ArrTypeFooBar

instance SmartCopy Bla where
    identifier = ID "BlaV1"
    version = 1
instance SmartCopy Spam where
    identifier = ID "SpamV1"
    version = 1  
instance SmartCopy Spam2 where
    identifier = ID "Spam2V1"
    version = 1
instance SmartCopy Some where
    identifier = ID "SomeV1"
    version = 1
instance SmartCopy Some2 where
    identifier = ID "Some2V1"
    version = 1
instance SmartCopy Bar where
    identifier = ID "BarV1"
    version = 1
instance SmartCopy Foo where
    identifier = ID "FooV1"
    version = 1
instance SmartCopy MyBool where
    identifier = ID "MyBoolV1"
    version = 1
instance SmartCopy FooBar where
    identifier = ID "FooBarV1"
    version = 1
instance SmartCopy MyDouble where
    identifier = ID "MyDoubleV1"
    version = 1
instance SmartCopy ArrType where
    identifier = ID "ArrTypeV1"
    version = 1
instance SmartCopy ArrTypeBar where
    identifier = ID "ArrTypeBarV1"
    version = 1
instance SmartCopy ArrTypeFooBar where
    identifier = ID "ArrTypeFooBarV1"
    version = 1  
instance SmartCopy MaybeTest where
    identifier = ID "MaybeTestV1"
    version = 1
instance SmartCopy MaybeTestX where
    identifier = ID "MaybeTestXV1"
    version = 1
instance SmartCopy BoolTest where
    identifier = ID "BoolTestV1"
    version = 1
instance SmartCopy BoolTestLong where
    identifier = ID "BoolTestLongV1"
    version = 1
instance SmartCopy StringTest where
    identifier = ID "StringTestV1"
    version = 1
instance SmartCopy StringTest2 where
    identifier = ID "StringTest2V1"
    version = 1
instance SmartCopy SumTest where
    identifier = ID "SumTestV1"
    version = 1

--------------------- Typeable instances ----------------------
-- for the sake of comparing types of fields in Generic instances
-- so that we can save bytes for those with an equal type.
-- Hopefully there is a way to avoid this.

deriving instance Typeable Bla 
deriving instance Typeable Spam 
deriving instance Typeable Spam2 
deriving instance Typeable Some 
deriving instance Typeable Some2 
deriving instance Typeable Bar 
deriving instance Typeable Foo 
deriving instance Typeable MyBool 
deriving instance Typeable FooBar 
deriving instance Typeable MyDouble 
deriving instance Typeable ArrType
deriving instance Typeable ArrTypeBar 
deriving instance Typeable ArrTypeFooBar 
deriving instance Typeable MaybeTest 
deriving instance Typeable MaybeTestX 
deriving instance Typeable BoolTest 
deriving instance Typeable BoolTestLong 
deriving instance Typeable StringTest 
deriving instance Typeable StringTest2 
deriving instance Typeable SumTest

sumtest1 = SumTest2 10
sumtest2 = SumTest3 10 5

some1 = Some (Spam 1) 0
some2 = Some2 (Spam2 1 2)

spam = Spam2 1 0

bar = BarLeft

maybeX = MaybeTestX [Nothing, Just 1] BarLeft ["Mal", "Wieder"]

mybool = MyTrue
mybool' = MyFalse

v1 = Foo 2 BarLeft
v2 = Foo 42 (BarRight (Foo 41 BarLeft))
v3 = Foo0 MyTrue (MyDouble 42.0)
v4 = Bar0 23 v3
v6a = ArrType []
v6b = ArrType [1,2,3,4]
v7 = ArrTypeBar [BarRight v1, BarLeft, BarRight v1]
v8 = ArrTypeFooBar [v3, v4]
maybetest1 = MaybeTest 42 (Just (BarRight (Foo 41 BarLeft)))
maybetest2 = MaybeTest 23 Nothing
booltest = BoolTest True
booltest' = BoolTestLong [True, False, True, True] False ["t", "e", "st!"]
string = StringTest "Test"
string' = StringTest2 "Test2" [1,2,3,4]
