{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
SC.deriveSafeCopy 1 'SC.base ''Easy

----------------------
-- SmartCopy instances
----------------------

instance SmartCopy MaybeTestX where
    version = 1
    readSmart fmt =
        readCons fmt [(C "MaybeTestX" (NF 3) False 0 False, readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              get3 <- getSmartGet fmt
                              l1 <- readField fmt get1 >>= either fail return
                              b <- readField fmt get2 >>= either fail return
                              l2 <- readField fmt get3 >>= either fail return
                              return $ Right $ MaybeTestX l1 b l2
    writeSmart fmt (MaybeTestX l1 b l2) =
        withCons fmt (C "MaybeTestX" (NF 3) False 0 False) withFields
        where withFields = do put1 <- getSmartPut fmt
                              put2 <- getSmartPut fmt
                              put3 <- getSmartPut fmt
                              withField fmt $ put1 l1
                              withField fmt $ put2 b
                              withField fmt $ put3 l2

instance SmartCopy MaybeTest where
    version = 1
    readSmart fmt =
        readCons fmt [(C "MaybeTest" (NF 2) False 0 False, readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              i <- readField fmt get1 >>= either fail return
                              m <- readField fmt get2 >>= either fail return
                              return $ Right $ MaybeTest i m
    writeSmart fmt (MaybeTest i m) =
        withCons fmt (C "MaybeTest" (NF 2) False 0 False) withFields
        where withFields = do put1 <- getSmartPut fmt
                              put2 <- getSmartPut fmt
                              withField fmt $ put1 i
                              withField fmt $ put2 m
           
instance SmartCopy BoolTest where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "BoolTest" (NF 1) False 0 False , readBool)]
        where readBool = do getter <- getSmartGet fmt
                            b :: Bool <- readField fmt getter >>= either fail return
                            return $ Right $ BoolTest b
    writeSmart fmt x@(BoolTest b) =
           withCons fmt (C "BoolTest" (NF 1) False 0 False) putB
           where putB = do putter <- getSmartPut fmt
                           withField fmt (putter b)

instance SmartCopy BoolTestLong where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "BoolTestLong" (LF ["blist", "b", "slist"]) False 0 False, readBT)]
        where readBT = do get1 <- getSmartGet fmt
                          get2 <- getSmartGet fmt
                          get3 <- getSmartGet fmt
                          blist <- readField fmt get1 >>= either fail return
                          b <- readField fmt get2 >>= either fail return
                          slist <- readField fmt get3 >>= either fail return
                          return $ Right $ BoolTestLong blist b slist
    writeSmart fmt x@(BoolTestLong blist b slist) =
        withCons fmt (C "BoolTestLong" (LF ["blist", "b", "slist"]) False 0 False) $
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
        readCons fmt [(C "StringTest" (NF 1) False 0 False, readString)]
        where readString = do getter <- getSmartGet fmt
                              s :: String <- readField fmt getter >>= either fail return
                              return $ Right $ StringTest s
    writeSmart fmt x@(StringTest s) =
        withCons fmt (C "StringTest" (NF 1) False 0 False) putStr
        where putStr = do putter <- getSmartPut fmt
                          withField fmt (putter s)

instance SmartCopy StringTest2 where
    version = 1
    readSmart fmt =
        readCons fmt [(C "StringTest2" (NF 2) False 0 False, readFields)]
        where readFields = do get1 <- getSmartGet fmt
                              get2 <- getSmartGet fmt
                              s :: String <- readField fmt get1 >>= either fail return
                              ints <- readField fmt get2 >>= either fail return
                              return $ Right $ StringTest2 s ints
    writeSmart fmt x@(StringTest2 s ints) =
        withCons fmt (C "StringTest2" (NF 2) False 0 False) writeFields
        where writeFields = do put1 <- getSmartPut fmt
                               put2 <- getSmartPut fmt
                               withField fmt $ put1 s
                               withField fmt $ put2 ints
                               

instance SmartCopy ArrType where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "ArrType" (NF 1) False 0 False, readInts)]
        where readInts = do getter <- getSmartGet fmt
                            ints <- readField fmt getter >>= either fail return
                            return $ Right $ ArrType ints

    writeSmart fmt x@(ArrType ints) =
        withCons fmt (C "ArrType" (NF 1) False 0 False) writePrimList
        where writePrimList = do putter <- getSmartPut fmt
                                 withField fmt $ putter ints

instance SmartCopy ArrTypeBar where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "ArrTypeBar" (NF 1) False 0 False, readBars)]
        where readBars = do getter <- getSmartGet fmt
                            bars <- readField fmt getter >>= either fail return
                            return $ Right $ ArrTypeBar bars

    writeSmart fmt x@(ArrTypeBar bars) =
        withCons fmt (C "ArrTypeBar" (NF 1) False 0 False) writeBarList
        where writeBarList = do putter <- getSmartPut fmt
                                withField fmt (putter bars)

instance SmartCopy ArrTypeFooBar where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "ArrTypeFooBar" (NF 1) False 0 False, readFbars)]
        where readFbars = do getter <- getSmartGet fmt
                             fbars <- readField fmt getter >>= either fail return
                             return $ Right $ ArrTypeFooBar fbars
    writeSmart fmt x@(ArrTypeFooBar fbars) =
        withCons fmt (C "ArrTypeFooBar" (NF 1) False 0 False) writeFBList
        where writeFBList = do putter <- getSmartPut fmt
                               withField fmt (putter fbars)

instance SmartCopy Foo where
    version = 1
    kind = base
    readSmart fmt =
      readCons fmt [(C "Foo" (NF 2) False 0 False, readFoo)]
      where readFoo =
                 do get1 <- getSmartGet fmt
                    get2 <- getSmartGet fmt
                    int <- readField fmt get1 >>= either fail return
                    bar <- readField fmt get2 >>= either fail return
                    return $ Right $ Foo int bar

    writeSmart fmt x@(Foo i bar) =
        withCons fmt (C "Foo" (NF 2) False 0 False) writeFields
        where writeFields = do put1 <- getSmartPut fmt
                               put2 <- getSmartPut fmt
                               withField fmt $ put1 i
                               withField fmt $ put2 bar

instance SmartCopy FooBar where
    version = 1
    kind = base
    readSmart fmt =
      readCons fmt [(C "Foo0" (NF 2) True 0 False, readFoo0),
                      (C "Bar0" (LF ["value", "foobar"]) True 1 False, readFoo1)]
      where
        readFoo0 =
               do get1 <- getSmartGet fmt
                  get2 <- getSmartGet fmt
                  myBool <- readField fmt get1 >>= either fail return
                  myDouble <- readField fmt get2 >>= either fail return
                  return $ Right $ Foo0 myBool myDouble
        readFoo1 = 
               do get1 <- getSmartGet fmt
                  get2 <- getSmartGet fmt
                  val <- readField fmt get1 >>= either fail return
                  foobar <- readField fmt get2 >>= either fail return
                  return $ Right $ Bar0 val foobar

    writeSmart fmt x@(Foo0 bool double) =
        withCons fmt (C "Foo0" (NF 2) True 0 False) writeFields
        where writeFields =
                do put1 <- getSmartPut fmt
                   put2 <- getSmartPut fmt
                   withField fmt $ put1 bool
                   withField fmt $ put2 double
    writeSmart fmt x@(Bar0 int foobar) =
        withCons fmt (C "Bar0" (LF ["value", "foobar"]) True 1 False) writeFields
        where writeFields =
                do put1 <- getSmartPut fmt
                   put2 <- getSmartPut fmt
                   withField fmt $ put1 int
                   withField fmt $ put2 foobar

instance SmartCopy MyDouble where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "MyDouble" (NF 1) False 0 False, readMyDouble)]
        where readMyDouble = do getter <- getSmartGet fmt
                                d <- readField fmt getter >>= either fail return
                                return $ Right $ MyDouble d
    writeSmart fmt x@(MyDouble d) =
        withCons fmt (C "MyDouble" (NF 1) False 0 False) writeD
        where writeD = do putter <- getSmartPut fmt
                          withField fmt (putter d)

instance SmartCopy MyBool where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "MyTrue" Empty True 1 False, return $ Right MyTrue)
                     ,(C "MyFalse" Empty True 0 False, return $ Right MyFalse)]
    writeSmart fmt MyTrue =
        withCons fmt (C "MyTrue" Empty True 1 False) $ return ()
    writeSmart fmt MyFalse =
        withCons fmt (C "MyFalse" Empty True 0 False) $ return ()

instance SmartCopy Easy where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "Easy" (NF 1) False 0 False, readEasy)]
        where
          readEasy =
              do getter <- getSmartGet fmt
                 f <- readField fmt getter >>= either fail return
                 return $ Right $ Easy f
    writeSmart fmt x@(Easy a) =
        withCons fmt (C "Easy" (NF 1) False 0 False) $ withField fmt writeInt
        where writeInt = do putter <- getSmartPut fmt
                            putter a

instance SmartCopy Bla where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "Bla" Empty False 0 False, return $ Right Bla)]
    writeSmart fmt bla =
        withCons fmt (C "Bla" Empty False 0 False) (return ())

instance SmartCopy Bar where
    version = 1
    kind = base
    writeSmart fmt (BarLeft) =
        withCons fmt (C "BarLeft" (NF 0) True 0 False) (return ())
    writeSmart fmt x@(BarRight foo) =
        withCons fmt (C "BarRight" (NF 1) True 1 False) writeFoo
        where writeFoo = do putter <- getSmartPut fmt
                            withField fmt $ putter foo
    readSmart fmt =
        readCons fmt [ (C "BarLeft" (NF 0) True 0 False, readBarLeft)
                       , (C "BarRight" (NF 1) True 1 False, readBarRight)]
        where readBarLeft = return $ Right BarLeft
              readBarRight = do getter <- getSmartGet fmt
                                f <- readField fmt getter >>= either fail return
                                return $ Right $ BarRight f 

instance SmartCopy Some2 where
    version = 1
    kind = base
    writeSmart fmt x@(Some2 spam') =
        withCons fmt (C "Some2" (NF 1) False 0 False) writeSpam
        where writeSpam = do putter <- getSmartPut fmt
                             withField fmt (putter spam')
    readSmart fmt =
        readCons fmt [(C "Some2" (NF 1) False 0 False, readSome2)]
        where readSome2 = do getSpam <- getSmartGet fmt
                             spam <- readField fmt getSpam >>= either fail return
                             return $ Right $ Some2 spam

instance SmartCopy Some where
    version = 1
    kind = base
    writeSmart fmt x@(Some spam int) =
        withCons fmt (C "Some" (NF 2) False 0 False) fields
           where fields = do putSpam <- getSmartPut fmt
                             putInt <- getSmartPut fmt
                             withField fmt (putSpam spam)
                             withField fmt (putInt int)
    readSmart fmt =
        readCons fmt [(C "Some" (NF 2) False 0 False, readSome)]
        where readSome = do getSpam <- getSmartGet fmt
                            getInt <- getSmartGet fmt
                            spam <- readField fmt getSpam >>= either fail return
                            int <- readField fmt getInt >>= either fail return
                            return $ Right $ Some spam int

                             
instance SmartCopy Spam where
    version = 1
    kind = base
    writeSmart fmt x@(Spam int) =
        withCons fmt (C "Spam" (NF 1) False 0 False) writeInt
        where writeInt = do putter <- getSmartPut fmt
                            withField fmt (putter int)
    readSmart fmt =
        readCons fmt [(C "Spam" (NF 1) False 0 False, readSpam)]
        where readSpam =
                do getI <- getSmartGet fmt
                   i <- readField fmt getI >>= either fail return
                   return $ Right $ Spam i

instance SmartCopy Spam2 where
    version = 1
    kind = base
    writeSmart fmt x@(Spam2 int1 int2) =
        withCons fmt (C "Spam2" (NF 2) False 0 False) writeFields
        where writeFields = 
               do putter <- getSmartPut fmt
                  withField fmt (putter int1)
                  withField fmt (putter int2)
    readSmart fmt =
        readCons fmt [(C "Spam2" (NF 2) False 0 False, readSpam2)]
        where readSpam2 =
                do getter <- getSmartGet fmt
                   i1 <- readField fmt getter >>= either fail return
                   i2 <- readField fmt getter >>= either fail return
                   return $ Right $ Spam2 i1 i2

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

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
