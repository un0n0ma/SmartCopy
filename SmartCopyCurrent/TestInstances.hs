{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInstances where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified JSON as J
import qualified StringFormat as S
import qualified XmlLikeFormat as X

import MonadTypesInstances
import SmartCopy

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
data Some' = Some' Spam' deriving (Eq, Show, Generic)
data Spam = Spam Int deriving (Eq, Show, Generic)
data Spam' = Spam' Int Int deriving (Eq, Show, Generic)
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
data StringTest' = StringTest' String [Int] deriving (Eq, Show, Generic)
data BoolTest = BoolTest Bool deriving (Eq, Show, Generic)
data BoolTest' = BoolTest' { blist :: [Bool], b :: Bool, slist :: [String] } 
    deriving (Eq, Show, Generic)
data MaybeTest = MaybeTest Int (Maybe Bar) deriving (Eq, Show, Generic)

instance Json.ToJSON Some
instance Json.ToJSON Some'
instance Json.ToJSON Spam
instance Json.ToJSON Spam'
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
instance Json.ToJSON StringTest'
instance Json.ToJSON BoolTest
instance Json.ToJSON BoolTest'
instance Json.FromJSON Some
instance Json.FromJSON Some'
instance Json.FromJSON Spam
instance Json.FromJSON Spam'
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
instance Json.FromJSON StringTest'
instance Json.FromJSON ArrTypeFooBar
instance Json.FromJSON BoolTest
instance Json.FromJSON BoolTest'

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
instance B.Serialize Spam'
instance B.Serialize Some
instance B.Serialize Some'
instance B.Serialize StringTest
instance B.Serialize StringTest'
instance B.Serialize BoolTest
instance B.Serialize BoolTest'
instance B.Serialize Easy

instance SC.SafeCopy Bla
instance SC.SafeCopy MyBool
instance SC.SafeCopy Spam
instance SC.SafeCopy Spam'
instance SC.SafeCopy Some
instance SC.SafeCopy Some'
instance SC.SafeCopy Bar
instance SC.SafeCopy Foo
instance SC.SafeCopy FooBar
instance SC.SafeCopy ArrType
instance SC.SafeCopy ArrTypeBar
instance SC.SafeCopy ArrTypeFooBar
instance SC.SafeCopy MyDouble
instance SC.SafeCopy StringTest
instance SC.SafeCopy StringTest'
instance SC.SafeCopy BoolTest
instance SC.SafeCopy BoolTest'
instance SC.SafeCopy Easy


----------------------
-- SmartCopy instances
----------------------

instance SmartCopy MaybeTest where
    readSmart fmt =
        readCons fmt [(C "MaybeTest" (NF 2) False 0, readFields)]
        where readFields = do i :: Int <- readField fmt $ smartGet fmt
                              m <- readField fmt $ smartGet fmt
                              return $ MaybeTest i m
    writeSmart fmt (MaybeTest i m) =
        withCons fmt (C "MaybeTest" (NF 2) False 0) withFields
        where withFields = do withField fmt (smartPut fmt i)
                              withField fmt (smartPut fmt m)
           
instance SmartCopy BoolTest where
    readSmart fmt =
        readCons fmt [(C "BoolTest" (NF 1) False 0 , readBool)]
        where readBool = do b :: Bool <- readField fmt $ smartGet fmt
                            return $ BoolTest b
    writeSmart fmt x@(BoolTest b) =
        withCons fmt (C "BoolTest" (NF 1) False 0) $ withField fmt (smartPut fmt b)

instance SmartCopy BoolTest' where
    readSmart fmt =
        readCons fmt [(C "BoolTest'" (LF ["blist", "b", "slist"]) False 0, readBT)]
        where readBT = do blist <- readField fmt $ readRepetition fmt 
                          b <- readField fmt $ smartGet fmt
                          slist <- readField fmt $ readRepetition fmt
                          return $ BoolTest' blist b slist
    writeSmart fmt x@(BoolTest' blist b slist) =
        withCons fmt (C "BoolTest'" (LF ["blist", "b", "slist"]) False 0) $
            do withField fmt $ withRepetition fmt blist
               withField fmt $ smartPut fmt b
               withField fmt $ withRepetition fmt slist

instance SmartCopy StringTest where
    readSmart fmt =
        readCons fmt [(C "StringTest" (NF 1) False 0, readString)]
        where readString = do s :: String <- readField fmt $ smartGet fmt
                              return $ StringTest s
    writeSmart fmt x@(StringTest s) =
        withCons fmt (C "StringTest" (NF 1) False 0) $ withField fmt (smartPut fmt s)

instance SmartCopy StringTest' where
    readSmart fmt =
        readCons fmt [(C "StringTest'" (NF 2) False 0, readFields)]
        where readFields = do s :: String <- readField fmt $ smartGet fmt
                              ints <- readField fmt $ readRepetition fmt
                              return $ StringTest' s ints
    writeSmart fmt x@(StringTest' s ints) =
        withCons fmt (C "StringTest'" (NF 2) False 0) writeFields
        where writeFields = do withField fmt (smartPut fmt s)
                               withField fmt $ withRepetition fmt ints
                               

instance SmartCopy ArrType where
    readSmart fmt =
        readCons fmt [(C "ArrType" (NF 1) False 0, readInts)]
        where readInts = do ints <- readField fmt $ readRepetition fmt
                            return $ ArrType ints

    writeSmart fmt x@(ArrType ints) =
        withCons fmt (C "ArrType" (NF 1) False 0) $ withField fmt writePrimList
        where writePrimList =
                withRepetition fmt ints

instance SmartCopy ArrTypeBar where
    readSmart fmt =
        readCons fmt [(C "ArrTypeBar" (NF 1) False 0, readBars)]
        where readBars = do bars <- readField fmt $ readRepetition fmt
                            return $ ArrTypeBar bars

    writeSmart fmt x@(ArrTypeBar bars) =
        withCons fmt (C "ArrTypeBar" (NF 1) False 0) $ withField fmt writeBarList
        where writeBarList =
                withRepetition fmt bars

instance SmartCopy ArrTypeFooBar where
    readSmart fmt =
        readCons fmt [(C "ArrTypeFooBar" (NF 1) False 0, readFbars)]
        where readFbars =
                  do fbars <- readField fmt $ readRepetition fmt
                     return $ ArrTypeFooBar fbars
    writeSmart fmt x@(ArrTypeFooBar fbars) =
        withCons fmt (C "ArrTypeFooBar" (NF 1) False 0) $ withField fmt writeFBList
        where writeFBList =
                withRepetition fmt fbars

instance SmartCopy Foo where
    readSmart fmt =
      readCons fmt [(C "Foo" (NF 2) False 0, readFoo)]
      where readFoo =
                 do int <- readField fmt $ smartGet fmt
                    bar <- readField fmt $ smartGet fmt
                    return $ Foo int bar

    writeSmart fmt x@(Foo i bar) =
        withCons fmt (C "Foo" (NF 2) False 0) writeFields
        where writeFields = do withField fmt (smartPut fmt i)
                               withField fmt (smartPut fmt bar)

instance SmartCopy FooBar where
    readSmart fmt =
      readCons fmt [(C "Foo0" (NF 2) True 0, readFoo0),
                      (C "Bar0" (LF ["value", "foobar"])  True 1, readFoo1)]
      where
        readFoo0 =
               do myBool <- readField fmt $ smartGet fmt
                  myDouble <- readField fmt $ smartGet fmt
                  return $ Foo0 myBool myDouble
        readFoo1 = 
               do val <- readField fmt $ smartGet fmt
                  foobar <- readField fmt $ smartGet fmt
                  return $ Bar0 val foobar

    writeSmart fmt x@(Foo0 bool double) =
        withCons fmt (C "Foo0" (NF 2) True 0) writeFields
        where writeFields =
                do withField fmt (smartPut fmt bool)
                   withField fmt (smartPut fmt double)
    writeSmart fmt x@(Bar0 int foobar) =
        withCons fmt (C "Bar0" (LF ["value", "foobar"]) True 1) writeFields
        where writeFields =
                do withField fmt (smartPut fmt int)
                   withField fmt (smartPut fmt foobar)

instance SmartCopy MyDouble where
    readSmart fmt =
        readCons fmt [(C "MyDouble" (NF 1) False 0, readMyDouble)]
        where readMyDouble = do d <- readField fmt $ smartGet fmt
                                return $ MyDouble d
    writeSmart fmt x@(MyDouble d) =
        withCons fmt (C "MyDouble" (NF 1) False 0) $ withField fmt $
                                                     smartPut fmt d

instance SmartCopy MyBool where
    readSmart fmt =
        readCons fmt [(C "MyTrue" Empty True 1, return MyTrue), (C "MyFalse" Empty True 0, return MyFalse)]
    writeSmart fmt MyTrue =
        withCons fmt (C "MyTrue" Empty True 1) $ return ()
    writeSmart fmt MyFalse =
        withCons fmt (C "MyFalse" Empty True 0) $ return ()

instance SmartCopy Easy where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "Easy" (NF 1) False 0, readEasy)]
        where
          readEasy =
              do f <- readField fmt $ smartGet fmt
                 return $ Easy f
    writeSmart fmt x@(Easy a) =
        withCons fmt (C "Easy" (NF 1) False 0) $ withField fmt $
        smartPut fmt a

instance SmartCopy Bla where
    version = 1
    kind = base
    readSmart fmt =
        readCons fmt [(C "Bla" Empty False 0, return Bla)]
    writeSmart fmt bla =
        withCons fmt (C "Bla" Empty False 0) (return ())

instance SmartCopy Bar where
    writeSmart fmt (BarLeft) =
        withCons fmt (C "BarLeft" (NF 0) True 0) (return ())
    writeSmart fmt x@(BarRight foo) =
        withCons fmt (C "BarRight" (NF 1) True 1) $
            withField fmt $ smartPut fmt foo
    readSmart fmt =
        readCons fmt [ (C "BarLeft" (NF 0) True 0, readBarLeft)
                       , (C "BarRight" (NF 1) True 1, readBarRight)]
        where readBarLeft = return BarLeft
              readBarRight = do f <- readField fmt (smartGet fmt)
                                return $ BarRight f 

instance SmartCopy Some' where
    writeSmart fmt x@(Some' spam') =
        withCons fmt (C "Some'" (NF 1) False 0) $ withField fmt (smartPut fmt spam')
    readSmart fmt =
        readCons fmt [(C "Some'" (NF 1) False 0, readSome')]
        where readSome' = do spam <- readField fmt (smartGet fmt)
                             return $ Some' spam

instance SmartCopy Some where
    version = 1
    kind = base
    writeSmart fmt x@(Some spam int) =
        withCons fmt (C "Some" (NF 2) False 0) fields
           where fields = do withField fmt (smartPut fmt spam)
                             withField fmt (smartPut fmt int)
    readSmart fmt =
        readCons fmt [(C "Some" (NF 2) False 0, readSome)]
        where readSome = do spam <- readField fmt (smartGet fmt)
                            int <- readField fmt (smartGet fmt)
                            return $ Some spam int

                             
instance SmartCopy Spam where
    writeSmart fmt x@(Spam int) =
        withCons fmt (C "Spam" (NF 1) False 0) $ withField fmt $
                         smartPut fmt int
    readSmart fmt =
        readCons fmt [(C "Spam" (NF 1) False 0, readSpam)]
        where readSpam =
                do i <- readField fmt (smartGet fmt)
                   return $ Spam i

instance SmartCopy Spam' where
    writeSmart fmt x@(Spam' int1 int2) =
        withCons fmt (C "Spam'" (NF 2) False 0) writeFields
        where writeFields = 
               do withField fmt (smartPut fmt int1)
                  withField fmt (smartPut fmt int2)
    readSmart fmt =
        readCons fmt [(C "Spam'" (NF 2) False 0, readSpam')]
        where readSpam' =
                do i1 <- readField fmt (smartGet fmt)
                   i2 <- readField fmt (smartGet fmt)
                   return $ Spam' i1 i2

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

v4 :: FooBar
v4 = Bar0 23 v3

v5 :: Bla
v5 = Bla

v6 :: ArrType
v6 = ArrType [1,2,3,4]

v7 :: ArrTypeBar
v7 = ArrTypeBar [BarRight v1, BarLeft]

v8 :: ArrTypeFooBar
v8 = ArrTypeFooBar [v3, v4]

some1 :: Some
some1 = Some (Spam 1) 2

some2 :: Some'
some2 = Some' (Spam' 1 2)

string :: StringTest
string = StringTest "Test"

string' :: StringTest'
string' = StringTest' "Test2" [1,2,3,4]

booltest = BoolTest True

booltest' = BoolTest' [True, False, True, True] False ["t", "e", "st!"]

maybetest1 = MaybeTest 42 (Just (BarRight (Foo 41 BarLeft)))

maybetest2 = MaybeTest 23 Nothing

---- Json Values

js1 :: Json.Value
js1 = Json.Object $ M.fromList [("version", Json.Number 0), ("object" , Json.Number 3)]

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
somestring2 = "Some' (Spam' (1) (2))"

s1 = "Easy (42)"
s2 = "Foo0 (MyTrue) (MyDouble (42.0))"

s3 = "Bar0 (23) (Foo0 (MyTrue) (MyDouble (42.0)))"

s4 = "ArrType ([1,2,3,4])"

s5 = "ArrTypeBar ([BarLeft, BarLeft])"

s6 = "ArrTypeBar ([BarRight (Foo (43) (BarLeft)), BarLeft])"
s6parsed = ArrTypeBar [BarRight (Foo 43 BarLeft), BarLeft]

s7 = "ArrTypeFooBar ([Foo0 (MyFalse) (MyDouble (32.1)), Bar0 (3) \
      \ (Foo0 (MyTrue) (MyDouble (1.0)))])"

---- Xml-like encoding

xml1 = "<Easy><0>42</0></Easy>"
xml2 = "<Foo0><0><MyTrue></MyTrue></0><1><MyDouble><0>42.0</0></MyDouble></1></Foo0>"
xml3 = "<Bla></Bla>"
xml4 = "<Some><0><Spam><0>1</0></Spam></0><1>2</1></Some>"
xml5 = "<Some'><0><Spam'><0>1</0><1>2</1></Spam'></0></Some'>"

main = do args <- getArgs
          let fmtList = ["json", "string", "xml"]
          case args of
            "json":_ ->
                do putStrLn "PARSING JSON Values:"
                   liftIO $ print (J.parseSmart js1 :: Fail Easy)
                   liftIO $ print (J.parseSmart js2 :: Fail MyDouble)
                   liftIO $ print (J.parseUnversioned js3 :: Fail FooBar)
                   putStrLn "DATATYPES as JSON Values:"
                   liftIO $ print (J.serializeSmart v3)
                   liftIO $ print (J.serializeSmart (MyDouble 23))
                   liftIO $ print (J.serializeSmart (Easy 42))
                   liftIO $ print (J.serializeSmart some1)
                   liftIO $ print (J.serializeSmart some2)                   
                   liftIO $ print (J.serializeSmart v1)
                   liftIO $ print (J.serializeSmart v2)
                   liftIO $ print (J.serializeUnversioned v4)
                   liftIO $ print (J.serializeSmart v4)
                   liftIO $ print (J.serializeSmart v6)
                   liftIO $ print (J.serializeSmart v7)
                   liftIO $ print (J.serializeSmart v8)
                   liftIO $ print (J.serializeSmart string)
                   liftIO $ print (J.serializeSmart string')
                   liftIO $ print (J.serializeSmart booltest)
                   liftIO $ print (J.serializeSmart booltest')
                   liftIO $ print (J.serializeSmart bar)
                   putStrLn "ENCODING:"
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v4))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v3))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v5))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v6))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v7))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v8))
            "string":_ ->
                do putStrLn "DATATYPES as String Values:"
                   liftIO $ print (S.serializeSmart v4)
                   liftIO $ print (S.serializeSmart (Easy 42))
                   liftIO $ print (S.serializeSmart v2)
                   liftIO $ print (S.serializeSmart (MyDouble 23))
                   liftIO $ print (S.serializeSmart v1)
                   liftIO $ print (S.serializeSmart v3)
                   liftIO $ print (S.serializeSmart v6)
                   liftIO $ print (S.serializeUnvers v7)
                   liftIO $ print (S.serializeUnvers v8)
                   liftIO $ print (S.serializeSmart some1)
                   liftIO $ print (S.serializeSmart some2)
                   liftIO $ print (S.serializeSmart s6parsed)
                   liftIO $ print (S.serializeSmart string)
                   liftIO $ print (S.serializeSmart string')
                   liftIO $ print (S.serializeSmart booltest)
                   liftIO $ print (S.serializeSmart booltest')
                   putStrLn "PARSING String Values:"
                   liftIO $ print (S.parseUnvers somestring1 :: Fail Some)
                   liftIO $ print (S.parseUnvers somestring2 :: Fail Some')
                   liftIO $ print (S.parseUnvers s1 :: Fail Easy)
                   liftIO $ print (S.parseUnvers s2 :: Fail FooBar)
                   liftIO $ print (S.parseUnvers s3 :: Fail FooBar)
                   liftIO $ print (S.parseUnvers s4 :: Fail ArrType)
                   liftIO $ print (S.parseUnvers s5 :: Fail ArrTypeBar)
                   liftIO $ print (S.parseUnvers s6 :: Fail ArrTypeBar)
                   liftIO $ print (S.parseUnvers s7 :: Fail ArrTypeFooBar)
            "xml":_ ->
                do putStrLn "DATATYPES xml-encoded:"
                   liftIO $ putStrLn (X.serializeSmart v5)
                   liftIO $ putStrLn (X.serializeSmart v2)
                   liftIO $ putStrLn (X.serializeSmart (MyDouble 23))
                   liftIO $ putStrLn (X.serializeSmart (Easy 42))
                   liftIO $ putStrLn (X.serializeSmart v1)
                   liftIO $ putStrLn (X.serializeSmart v3)
                   liftIO $ putStrLn (X.serializeSmart v4)
                   liftIO $ putStrLn (X.serializeSmart v6)
                   liftIO $ putStrLn (X.serializeSmart v7)
                   liftIO $ putStrLn (X.serializeSmart v8)
                   liftIO $ putStrLn (X.serializeSmart some1)
                   liftIO $ putStrLn (X.serializeSmart some2)
                   liftIO $ putStrLn (X.serializeSmart s6parsed)
                   liftIO $ putStrLn (X.serializeSmart string)
                   liftIO $ putStrLn (X.serializeSmart string')
                   liftIO $ putStrLn (X.serializeSmart booltest)
                   liftIO $ putStrLn (X.serializeSmart booltest')
                   putStrLn "PARSING XML:"
                   liftIO $ print (X.parseSmart xml1 :: Fail Easy)
                   liftIO $ print (X.parseSmart xml3 :: Fail Bla)
                   liftIO $ print (X.parseSmart xml4 :: Fail Some)
                   liftIO $ print (X.parseSmart xml2 :: Fail FooBar)
                   liftIO $ print (X.parseSmart xml5 :: Fail Some')
            _  ->
                putStrLn $ "You need to specify a format out of " ++ show fmtList
                             
                


