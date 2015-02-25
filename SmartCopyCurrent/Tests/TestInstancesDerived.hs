{-# LANGUAGE DeriveGeneric #-}

module Tests.TestInstancesDerived where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Generic
import SmartCopy.Instances
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import GHC.Generics

data Bla = Bla deriving (Show, Eq, Generic)
data Spam2 = Spam2 Int Int deriving (Show, Eq, Generic)
data Spam = Spam Int deriving (Show, Eq, Generic)
data Some2 = Some2 Spam2 deriving (Show, Eq, Generic)
data Some = Some Spam Int deriving (Show, Eq, Generic)
data Bar = BarLeft | BarRight Foo deriving (Show, Eq, Generic)
data Foo = Foo Int Bar deriving (Eq, Show, Generic)
data MyBool = MyFalse | MyTrue deriving (Eq, Show, Generic)

instance SmartCopy Bla where version = 1
instance SmartCopy Spam where version = 1
instance SmartCopy Spam2 where version = 1
instance SmartCopy Some where version = 1
instance SmartCopy Some2 where version = 1
instance SmartCopy Bar where version = 1
instance SmartCopy Foo where version = 1
instance SmartCopy MyBool where version = 1

some1 = Some (Spam 1) 2
some2 = Some2 (Spam2 1 2)

bar = BarLeft

mybool = MyTrue
mybool' = MyFalse

v1 = Foo 2 BarLeft
v2 = Foo 42 (BarRight (Foo 41 BarLeft))
