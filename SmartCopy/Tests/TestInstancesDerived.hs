{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tests.TestInstancesDerived where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.SmartCopy

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

instance SmartCopy Bla where version = 1
instance SmartCopy Spam where version = 1  
instance SmartCopy Spam2 where version = 1
instance SmartCopy Some where version = 1
instance SmartCopy Some2 where version = 1
instance SmartCopy Bar where version = 1
instance SmartCopy Foo where version = 1
instance SmartCopy MyBool where version = 1
instance SmartCopy FooBar where version = 1
instance SmartCopy MyDouble where version = 1
instance SmartCopy ArrTypeBar where version = 1
instance SmartCopy ArrTypeFooBar where version = 1  
instance SmartCopy MaybeTest where version = 1
instance SmartCopy MaybeTestX where version = 1
instance SmartCopy BoolTest where version = 1
instance SmartCopy BoolTestLong where version = 1
instance SmartCopy StringTest where version = 1
instance SmartCopy StringTest2 where version = 1
instance SmartCopy SumTest where version = 1

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

some1 = Some (Spam 1) 2
some2 = Some2 (Spam2 1 2)

spam = Spam2 1 2

bar = BarLeft

maybeX = MaybeTestX [Nothing, Just 1] BarLeft ["Mal", "Wieder"]

mybool = MyTrue
mybool' = MyFalse

v1 = Foo 2 BarLeft
v2 = Foo 42 (BarRight (Foo 41 BarLeft))
v3 = Foo0 MyTrue (MyDouble 42.0)
v4 = Bar0 23 v3
v7 = ArrTypeBar [BarRight v1, BarLeft, BarRight v1]
v8 = ArrTypeFooBar [v3, v4]
maybetest1 = MaybeTest 42 (Just (BarRight (Foo 41 BarLeft)))
maybetest2 = MaybeTest 23 Nothing
booltest = BoolTest True
booltest' = BoolTestLong [True, False, True, True] False ["t", "e", "st!"]
string = StringTest "Test"
string' = StringTest2 "Test2" [1,2,3,4]
