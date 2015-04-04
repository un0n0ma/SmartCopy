{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.TestInstances where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Instances
import SmartCopy.MonadTypesInstances
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as M
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B
import qualified Data.Vector as V

import GHC.Generics
import System.Environment
-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Reader hiding (sequence)

import Control.Applicative
import Control.Monad
import Data.Typeable

deriving instance Typeable Some
deriving instance Typeable Some2
deriving instance Typeable Spam
deriving instance Typeable Spam2
deriving instance Typeable Foo
deriving instance Typeable Bar
deriving instance Typeable Easy
deriving instance Typeable FooBar
deriving instance Typeable MyDouble
deriving instance Typeable MyBool
deriving instance Typeable Bla
deriving instance Typeable ArrType
deriving instance Typeable ArrTypeBar
deriving instance Typeable ArrTypeFooBar
deriving instance Typeable StringTest
deriving instance Typeable StringTest2
deriving instance Typeable BoolTest
deriving instance Typeable BoolTestLong
deriving instance Typeable MaybeTest
deriving instance Typeable MaybeTestX
deriving instance Typeable SumTest

data Some = Some Spam Int deriving (Eq, Show, Generic)
data Some2 = Some2 Spam2 deriving (Eq, Show, Generic)
data Spam = Spam Int deriving (Eq, Show, Generic)
data Spam2 = Spam2 Int Int deriving (Eq, Show, Generic)
data Foo = Foo Int Bar deriving (Eq, Show, Generic)
data Bar = BarLeft | BarRight Foo deriving (Eq, Show, Generic)
data Easy = Easy Int deriving (Eq, Show, Generic)
data FooBar = Foo0 MyBool MyDouble | Bar0 { value :: Int, foobar :: FooBar }
    deriving (Eq, Show, Generic)
data MyDouble = MyDouble Double deriving (Eq, Show, Generic)
data MyBool = MyFalse | MyTrue deriving (Eq, Show, Generic)
data Bla = Bla deriving (Eq, Show, Generic)
data ArrType = ArrType [Int] deriving (Eq, Show, Generic)
data ArrTypeBar = ArrTypeBar [Bar] deriving (Eq, Show, Generic)
data ArrTypeFooBar = ArrTypeFooBar [FooBar] deriving (Eq, Show, Generic)
data StringTest = StringTest String deriving (Eq, Show, Generic)
data StringTest2 = StringTest2 String [Int] deriving (Eq, Show, Generic)
data BoolTest = BoolTest Bool deriving (Eq, Show, Generic)
data BoolTestLong = BoolTestLong { blist :: [Bool], b :: Bool, slist :: [String] } 
    deriving (Eq, Show, Generic)
data MaybeTest = MaybeTest Int (Maybe Bar) deriving (Eq, Show, Generic)
data MaybeTestX = MaybeTestX [Maybe Int] Bar [String] deriving (Eq, Show, Generic)
data SumTest = SumTest1 | SumTest2 Int | SumTest3 Int Int deriving (Eq, Show, Generic)

instance Json.ToJSON Some
instance Json.ToJSON Some2
instance Json.ToJSON Spam
instance Json.ToJSON Spam2
instance Json.ToJSON Bar
instance Json.ToJSON Foo
instance Json.ToJSON FooBar
instance Json.ToJSON MyDouble
instance Json.ToJSON MyBool
instance Json.ToJSON Bla
instance Json.ToJSON Easy
instance Json.ToJSON ArrType
instance Json.ToJSON ArrTypeBar
instance Json.ToJSON ArrTypeFooBar
instance Json.ToJSON StringTest
instance Json.ToJSON StringTest2
instance Json.ToJSON BoolTest
instance Json.ToJSON BoolTestLong
instance Json.ToJSON SumTest
instance Json.ToJSON MaybeTestX
instance Json.FromJSON Some
instance Json.FromJSON Some2
instance Json.FromJSON Spam
instance Json.FromJSON Spam2
instance Json.FromJSON Bar
instance Json.FromJSON Foo
instance Json.FromJSON FooBar
instance Json.FromJSON MyDouble
instance Json.FromJSON MyBool
instance Json.FromJSON Bla
instance Json.FromJSON Easy
instance Json.FromJSON ArrType
instance Json.FromJSON ArrTypeBar
instance Json.FromJSON StringTest
instance Json.FromJSON StringTest2
instance Json.FromJSON ArrTypeFooBar
instance Json.FromJSON BoolTest
instance Json.FromJSON BoolTestLong
instance Json.FromJSON SumTest
instance Json.FromJSON MaybeTestX

instance B.Serialize Bla
instance B.Serialize Foo
instance B.Serialize Bar
instance B.Serialize MyDouble
instance B.Serialize MyBool
instance B.Serialize FooBar
instance B.Serialize ArrType
instance B.Serialize ArrTypeBar
instance B.Serialize ArrTypeFooBar
instance B.Serialize Spam
instance B.Serialize Spam2
instance B.Serialize Some
instance B.Serialize Some2
instance B.Serialize StringTest
instance B.Serialize StringTest2
instance B.Serialize BoolTest
instance B.Serialize BoolTestLong
instance B.Serialize Easy
instance B.Serialize MaybeTest
instance B.Serialize MaybeTestX
instance B.Serialize SumTest

SC.deriveSafeCopy 1 'SC.base ''Bla
SC.deriveSafeCopy 1 'SC.base ''MyBool
SC.deriveSafeCopy 1 'SC.base ''Spam
SC.deriveSafeCopy 1 'SC.base ''Spam2
SC.deriveSafeCopy 1 'SC.base ''Some
SC.deriveSafeCopy 1 'SC.base ''Some2
SC.deriveSafeCopy 1 'SC.base ''Bar
SC.deriveSafeCopy 1 'SC.base ''Foo
SC.deriveSafeCopy 1 'SC.base ''FooBar
SC.deriveSafeCopy 1 'SC.base ''ArrType
SC.deriveSafeCopy 1 'SC.base ''ArrTypeBar
SC.deriveSafeCopy 1 'SC.base ''ArrTypeFooBar
SC.deriveSafeCopy 1 'SC.base ''MyDouble
SC.deriveSafeCopy 1 'SC.base ''StringTest
SC.deriveSafeCopy 1 'SC.base ''StringTest2
SC.deriveSafeCopy 1 'SC.base ''BoolTest
SC.deriveSafeCopy 1 'SC.base ''BoolTestLong
SC.deriveSafeCopy 1 'SC.base ''MaybeTest
SC.deriveSafeCopy 1 'SC.base ''MaybeTestX
SC.deriveSafeCopy 1 'SC.base ''Easy
SC.deriveSafeCopy 1 'SC.base ''SumTest

----------------------
-- SmartCopy instances
----------------------

instance SmartCopy MaybeTestX where
    version = 1
    readSmart fmt =
        readCons fmt [(CInfo "MaybeTestX" (NF 3) False 0 , readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              get3 <- getSmartGet fmt
                              l1 <- readField fmt get1
                              b <- readField fmt get2
                              l2 <- readField fmt get3
                              return $ MaybeTestX l1 b l2
    writeSmart fmt (MaybeTestX l1 b l2) =
        withCons fmt (CInfo "MaybeTestX" (NF 3) False 0) withFields
        where withFields = do put1 <- getSmartPut fmt
                              put2 <- getSmartPut fmt
                              put3 <- getSmartPut fmt
                              withField fmt $ put1 l1
                              withField fmt $ put2 b
                              withField fmt $ put3 l2

instance SmartCopy MaybeTest where
    version = 1
    readSmart fmt =
        readCons fmt [(CInfo "MaybeTest" (NF 2) False 0 , readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              i <- readField fmt get1
                              m <- readField fmt get2
                              return $ MaybeTest i m
    writeSmart fmt (MaybeTest i m) =
        withCons fmt (CInfo "MaybeTest" (NF 2) False 0) withFields
        where withFields = do put1 <- getSmartPut fmt
                              put2 <- getSmartPut fmt
                              withField fmt $ put1 i
                              withField fmt $ put2 m
           
instance SmartCopy BoolTest where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "BoolTest" (NF 1) False 0 , readBool)]
        where readBool = do getter <- getSmartGet fmt
                            b :: Bool <- readField fmt getter
                            return $ BoolTest b
    writeSmart fmt x@(BoolTest b) =
           withCons fmt (CInfo "BoolTest" (NF 1) False 0) putB
           where putB = do putter <- getSmartPut fmt
                           withField fmt (putter b)

instance SmartCopy BoolTestLong where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "BoolTestLong" (LF ["blist", "b", "slist"]) False 0 , readBT)]
        where readBT = do get1 <- getSmartGet fmt
                          get2 <- getSmartGet fmt
                          get3 <- getSmartGet fmt
                          blist <- readField fmt get1
                          b <- readField fmt get2
                          slist <- readField fmt get3
                          return $ BoolTestLong blist b slist
    writeSmart fmt x@(BoolTestLong blist b slist) =
        withCons fmt (CInfo "BoolTestLong" (LF ["blist", "b", "slist"]) False 0) $
            do put1 <- getSmartPut fmt
               put2 <- getSmartPut fmt
               put3 <- getSmartPut fmt
               withField fmt $ put1 blist
               withField fmt $ put2 b
               withField fmt $ put3 slist

instance SmartCopy StringTest where
    version = 1
    kind = base 
    readSmart fmt =
        readCons fmt [(CInfo "StringTest" (NF 1) False 0 , readString)]
        where readString = do getter <- getSmartGet fmt
                              s :: String <- readField fmt getter
                              return $ StringTest s
    writeSmart fmt x@(StringTest s) =
        withCons fmt (CInfo "StringTest" (NF 1) False 0) putStr
        where putStr = do putter <- getSmartPut fmt
                          withField fmt (putter s)

instance SmartCopy StringTest2 where
    version = 1
    readSmart fmt =
        readCons fmt [(CInfo "StringTest2" (NF 2) False 0 , readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              s :: String <- readField fmt get1
                              ints <- readField fmt get2
                              return $ StringTest2 s ints
    writeSmart fmt x@(StringTest2 s ints) =
        withCons fmt (CInfo "StringTest2" (NF 2) False 0) writeFields
        where writeFields = do put1 <- getSmartPut fmt
                               put2 <- getSmartPut fmt
                               withField fmt $ put1 s
                               withField fmt $ put2 ints
                               

instance SmartCopy ArrType where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "ArrType" (NF 1) False 0 , readInts)]
        where readInts = do getter <- getSmartGet fmt
                            ints <- readField fmt getter
                            return $ ArrType ints

    writeSmart fmt x@(ArrType ints) =
        withCons fmt (CInfo "ArrType" (NF 1) False 0) writePrimList
        where writePrimList = do putter <- getSmartPut fmt
                                 withField fmt $ putter ints

instance SmartCopy ArrTypeBar where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "ArrTypeBar" (NF 1) False 0 , readBars)]
        where readBars = do getter <- getSmartGet fmt
                            bars <- readField fmt getter
                            return $ ArrTypeBar bars

    writeSmart fmt x@(ArrTypeBar bars) =
        withCons fmt (CInfo "ArrTypeBar" (NF 1) False 0) writeBarList
        where writeBarList = do putter <- getSmartPut fmt
                                withField fmt (putter bars)

instance SmartCopy ArrTypeFooBar where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "ArrTypeFooBar" (NF 1) False 0 , readFbars)]
        where readFbars = do getter <- getSmartGet fmt
                             fbars <- readField fmt getter
                             return $ ArrTypeFooBar fbars
    writeSmart fmt x@(ArrTypeFooBar fbars) =
        withCons fmt (CInfo "ArrTypeFooBar" (NF 1) False 0) writeFBList
        where writeFBList = do putter <- getSmartPut fmt
                               withField fmt (putter fbars)

instance SmartCopy Foo where
    version = 1
    kind = base
    readSmart fmt =
      readCons fmt [(CInfo "Foo" (NF 2) False 0 , readFoo)]
      where readFoo =
                 do get1 <- getSmartGet fmt
                    get2 <- getSmartGet fmt
                    int <- readField fmt get1
                    bar <- readField fmt get2
                    return $ Foo int bar

    writeSmart fmt x@(Foo i bar) =
        withCons fmt (CInfo "Foo" (NF 2) False 0) writeFields
        where writeFields = do put1 <- getSmartPut fmt
                               put2 <- getSmartPut fmt
                               withField fmt $ put1 i
                               withField fmt $ put2 bar

instance SmartCopy FooBar where
    version = 1
    kind = base
    readSmart fmt =
      readCons fmt [(CInfo "Foo0" (NF 2) True 0 , readFoo0),
                      (CInfo "Bar0" (LF ["value", "foobar"]) True 1 , readFoo1)]
      where
        readFoo0 =
               do get1 <- getSmartGet fmt
                  get2 <- getSmartGet fmt
                  myBool <- readField fmt get1
                  myDouble <- readField fmt get2
                  return $ Foo0 myBool myDouble
        readFoo1 = 
               do get1 <- getSmartGet fmt
                  get2 <- getSmartGet fmt
                  val <- readField fmt get1
                  foobar <- readField fmt get2
                  return $ Bar0 val foobar

    writeSmart fmt x@(Foo0 bool double) =
        withCons fmt (CInfo "Foo0" (NF 2) True 0) writeFields
        where writeFields =
                do put1 <- getSmartPut fmt
                   put2 <- getSmartPut fmt
                   withField fmt $ put1 bool
                   withField fmt $ put2 double
    writeSmart fmt x@(Bar0 int foobar) =
        withCons fmt (CInfo "Bar0" (LF ["value", "foobar"]) True 1) writeFields
        where writeFields =
                do put1 <- getSmartPut fmt
                   put2 <- getSmartPut fmt
                   withField fmt $ put1 int
                   withField fmt $ put2 foobar

instance SmartCopy MyDouble where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "MyDouble" (NF 1) False 0 , readMyDouble)]
        where readMyDouble = do getter <- getSmartGet fmt
                                d <- readField fmt getter
                                return $ MyDouble d
    writeSmart fmt x@(MyDouble d) =
        withCons fmt (CInfo "MyDouble" (NF 1) False 0) writeD
        where writeD = do putter <- getSmartPut fmt
                          withField fmt (putter d)

instance SmartCopy MyBool where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "MyTrue" Empty True 1 , return MyTrue)
                     ,(CInfo "MyFalse" Empty True 0 , return MyFalse)]
    writeSmart fmt MyTrue =
        withCons fmt (CInfo "MyTrue" Empty True 1) $ return ()
    writeSmart fmt MyFalse =
        withCons fmt (CInfo "MyFalse" Empty True 0) $ return ()

instance SmartCopy Easy where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "Easy" (NF 1) False 0 , readEasy)]
        where
          readEasy =
              do getter <- getSmartGet fmt
                 f <- readField fmt getter
                 return $ Easy f
    writeSmart fmt x@(Easy a) =
        withCons fmt (CInfo "Easy" (NF 1) False 0) $ withField fmt writeInt
        where writeInt = do putter <- getSmartPut fmt
                            putter a

instance SmartCopy Bla where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(CInfo "Bla" Empty False 0 , return Bla)]
    writeSmart fmt bla =
        withCons fmt (CInfo "Bla" Empty False 0) (return ())

instance SmartCopy Bar where
    version = 1
    kind = base
    writeSmart fmt (BarLeft) =
        withCons fmt (CInfo "BarLeft" (NF 0) True 0) (return ())
    writeSmart fmt x@(BarRight foo) =
        withCons fmt (CInfo "BarRight" (NF 1) True 1) writeFoo
        where writeFoo = do putter <- getSmartPut fmt
                            withField fmt $ putter foo
    readSmart fmt =
        readCons fmt [ (CInfo "BarLeft" (NF 0) True 0 , readBarLeft)
                       , (CInfo "BarRight" (NF 1) True 1 , readBarRight)]
        where readBarLeft = return BarLeft
              readBarRight = do getter <- getSmartGet fmt
                                f <- readField fmt getter
                                return $ BarRight f 

instance SmartCopy Some2 where
    version = 1
    kind = base
    writeSmart fmt x@(Some2 spam') =
        withCons fmt (CInfo "Some2" (NF 1) False 0) writeSpam
        where writeSpam = do putter <- getSmartPut fmt
                             withField fmt (putter spam')
    readSmart fmt =
        readCons fmt [(CInfo "Some2" (NF 1) False 0 , readSome2)]
        where readSome2 = do getSpam <- getSmartGet fmt
                             spam <- readField fmt getSpam
                             return $ Some2 spam

instance SmartCopy Some where
    version = 1
    kind = base
    writeSmart fmt x@(Some spam int) =
        withCons fmt (CInfo "Some" (NF 2) False 0) fields
           where fields = do putSpam <- getSmartPut fmt
                             putInt <- getSmartPut fmt
                             withField fmt (putSpam spam)
                             withField fmt (putInt int)
    readSmart fmt =
        readCons fmt [(CInfo "Some" (NF 2) False 0 , readSome)]
        where readSome = do getSpam <- getSmartGet fmt
                            getInt <- getSmartGet fmt
                            spam <- readField fmt getSpam
                            int <- readField fmt getInt
                            return $ Some spam int

                             
instance SmartCopy Spam where
    version = 1
    kind = base
    writeSmart fmt x@(Spam int) =
        withCons fmt (CInfo "Spam" (NF 1) False 0) writeInt
        where writeInt = do putter <- getSmartPut fmt
                            withField fmt (putter int)
    readSmart fmt =
        readCons fmt [(CInfo "Spam" (NF 1) False 0 , readSpam)]
        where readSpam =
                do getI <- getSmartGet fmt
                   i <- readField fmt getI
                   return $ Spam i

instance SmartCopy Spam2 where
    version = 1
    kind = base
    writeSmart fmt x@(Spam2 int1 int2) =
        withCons fmt (CInfo "Spam2" (NF 2) False 0) writeFields
        where writeFields = 
               do putter <- getSmartPut fmt
                  withField fmt (putter int1)
                  withField fmt (putter int2)
    readSmart fmt =
        readCons fmt [(CInfo "Spam2" (NF 2) False 0 , readSpam2)]
        where readSpam2 =
                do getter <- getSmartGet fmt
                   i1 <- readField fmt getter
                   i2 <- readField fmt getter
                   return $ Spam2 i1 i2

instance SmartCopy SumTest where
    version = 1
    writeSmart fmt x@SumTest1 =
        withCons fmt (CInfo "SumTest1" (NF 0) True 0) $ return ()
    writeSmart fmt x@(SumTest2 int) =
        withCons fmt (CInfo "SumTest2" (NF 1) True 1) writeFields
        where writeFields = do putter <- getSmartPut fmt
                               withField fmt $ putter int
    writeSmart fmt x@(SumTest3 int1 int2) =
        withCons fmt (CInfo "SumTest3" (NF 2) True 2) writeFields
        where writeFields = do putter <- getSmartPut fmt
                               withField fmt $ putter int1
                               withField fmt $ putter int2
    readSmart fmt =
        readCons fmt [(CInfo "SumTest1" (NF 0) True 0 , readSum1)
                     ,(CInfo "SumTest2" (NF 1) True 1 , readSum2)
                     ,(CInfo "SumTest3" (NF 2) True 2 , readSum3)
                     ]
        where readSum1 = return SumTest1
              readSum2 =
                do getter <- getSmartGet fmt
                   i1 <- readField fmt getter
                   return $ SumTest2 i1
              readSum3 =
                do getter <- getSmartGet fmt
                   i1 <- readField fmt getter
                   i2 <- readField fmt getter
                   return $ SumTest3 i1 i2
        
-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

sumtest1 = SumTest2 10
sumtest2 = SumTest3 10 5

bar = BarLeft

mybool :: MyBool
mybool = MyTrue

mybool' = MyFalse

v1 :: Foo
v1 = Foo 2 BarLeft

v2 :: Foo
v2 = Foo 42 (BarRight (Foo 41 BarLeft))

v3 :: FooBar
v3 = Foo0 MyTrue (MyDouble 42.0) 
v3' = Foo0 MyFalse (MyDouble 42.0) 

v4 :: FooBar
v4 = Bar0 23 v3

v5 :: Bla
v5 = Bla

v6a :: ArrType
v6a = ArrType []

v6b :: ArrType
v6b = ArrType [1,2,3,4]

v7 :: ArrTypeBar
v7 = ArrTypeBar [BarRight v1, BarLeft, BarRight v1]

v8 :: ArrTypeFooBar
v8 = ArrTypeFooBar [v3,v4]

some1 :: Some
some1 = Some (Spam 1) 2

some2 :: Some2
some2 = Some2 (Spam2 1 2)

spam :: Spam2
spam = Spam2 1 2

string :: StringTest
string = StringTest "Test"

string' :: StringTest2
string' = StringTest2 "Test2" [1,2,3,4]

booltest = BoolTest True

booltest' = BoolTestLong [True, False, True, True] False ["t", "e", "st!"]

maybetest1 = MaybeTest 42 (Just (BarRight (Foo 41 BarLeft)))

maybetest2 = MaybeTest 23 Nothing

maybeX = MaybeTestX [Nothing, Just 1] BarLeft ["Mal", "Wieder"]

---- Json Values

js1 :: Json.Value
js1 = Json.Object $ M.fromList [("version", Json.Number 1), ("object" , Json.Array $
                    V.fromList [Json.Object $
                    M.fromList [("version", Json.Number 0), ("object", Json.Number 3)]])]

js2 :: Json.Value
js2 = Json.Number 2.3

js3 :: Json.Value
js3 = Json.Object $ M.fromList
      [ ("foobar", Json.Object $ M.fromList
      [ ("tag", Json.String "Foo0")
      , ("contents", Json.Array $ V.fromList
      [ Json.String "MyTrue", Json.Number 42 ])])
      , ("tag", Json.String "Bar0")
      , ("value", Json.Number 23) ]

---- Strings

somestring1 = "Some (Spam (1)) (2)"
somestring2 = "Some2 (Spam2 (1) (2))"

s1 = "Easy (42)"
s2 = "Foo0 (MyTrue) (MyDouble (42.0))"

s3 = "Bar0 (23) (Foo0 (MyTrue) (MyDouble (42.0)))"

s4 = "ArrType ([1,2,3,4])"

s5 = "ArrTypeBar ([BarLeft,BarLeft])"

s6 = "ArrTypeBar ([BarRight (Foo (43) (BarLeft)),BarLeft])"
s6parsed = ArrTypeBar [BarRight (Foo 43 BarLeft), BarLeft]

s7 = "ArrTypeFooBar ([Foo0 (MyFalse) (MyDouble (32.1)),Bar0 (3) (Foo0 (MyTrue) (MyDouble (1.0)))])"

---- Xml-like encoding

xml1 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Easy><0>42</0></Easy>"
xml2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Foo0><0><MyTrue/></0><1><MyDouble><0>42.0</0></MyDouble></1></Foo0>"
xml3 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Bla/>"
xml4 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Some><0><Spam><0>1</0></Spam></0><1>2</1></Some>"
xml5 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Some2><0><Spam2><0>1</0><1>2</1></Spam2></0></Some2>"
