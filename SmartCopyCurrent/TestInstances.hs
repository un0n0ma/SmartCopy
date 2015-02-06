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


----------------------
-- SmartCopy instances
----------------------

instance SmartCopy ArrType where
    readSmart fmt =
        readCons fmt [((C "ArrType" (Left 1) False 0), readInts)]
        where readInt = do i :: Int <- readSmart fmt
                           return i
              readInts = do ints <- readField fmt $ readRepetition fmt $ readInt
                            return $ ArrType ints

    writeSmart fmt (ArrType ints) =
        withCons fmt (C "ArrType" (Left 1) False 0) $ withField fmt writePrimList
        where writePrimList =
                withRepetition fmt (\i -> writePrimitive fmt $ PrimInt i) ints

instance SmartCopy ArrTypeBar where
    readSmart fmt =
        readCons fmt [((C "ArrTypeBar" (Left 1) False 0), readBars)]
        where readBars = do bars <- readField fmt $ readRepetition fmt $ readSmart fmt
                            return $ ArrTypeBar bars

    writeSmart fmt (ArrTypeBar bars) =
        withCons fmt (C "ArrTypeBar" (Left 1) False 0) $ withField fmt writeBarList
        where writeBarList =
                withRepetition fmt (\b -> writeSmart fmt b) bars

instance SmartCopy ArrTypeFooBar where
    readSmart fmt =
        readCons fmt [(C "ArrTypeFooBar" (Left 1) False 0, readFbars)]
        where readFbars =
                  do fbars <- readField fmt $ readRepetition fmt $ readSmart fmt
                     return $ ArrTypeFooBar fbars
    writeSmart fmt (ArrTypeFooBar fbars) =
        withCons fmt (C "ArrTypeFooBar" (Left 1) False 0) $ withField fmt writeFBList
        where writeFBList =
                withRepetition fmt (\b -> writeSmart fmt b) fbars

instance SmartCopy Foo where
    readSmart fmt =
      do readCons fmt [(C "Foo" (Left 2) False 0, readFoo)]
      where readFoo =
                 do int <- readField fmt $ readSmart fmt
                    bar <- readField fmt $ readSmart fmt
                    return $ Foo int bar

    writeSmart fmt (Foo i bar) =
        do withCons fmt (C "Foo" (Left 2) False 0) writeFields
        where writeFields = do withField fmt (writePrimitive fmt $ PrimInt i)
                               withField fmt (writeSmart fmt bar)

instance SmartCopy FooBar where
    readSmart fmt =
      do readCons fmt [(C "Foo0" (Left 2) True 0, readFoo0),
                         ((C "Bar0" (Right ["value", "foobar"])  True 1), readFoo1)]
      where
        readFoo0 =
               do myBool <- readField fmt $ readSmart fmt
                  myDouble <- readField fmt $ readSmart fmt
                  return $ Foo0 myBool myDouble
        readFoo1 = 
               do val <- readField fmt $ readSmart fmt
                  foobar <- readField fmt $ readSmart fmt
                  return $ Bar0 val foobar

    writeSmart fmt (Foo0 bool double) =
        do withCons fmt (C "Foo0" (Left 2) True 0) writeFields
        where writeFields =
                do withField fmt (writeSmart fmt bool)
                   withField fmt (writeSmart fmt double)
    writeSmart fmt (Bar0 int foobar) =
        do withCons fmt (C "Bar0" (Right ["value", "foobar"]) True 1) writeFields
        where writeFields =
                do withField fmt (writeSmart fmt int)
                   withField fmt (writeSmart fmt foobar)


instance SmartCopy MyDouble where
    readSmart fmt =
        do readCons fmt [((C "MyDouble" (Left 1) False 0), readMyDouble)]
           where readMyDouble = do d <- readField fmt $ readSmart fmt
                                   return $ MyDouble d
    writeSmart fmt (MyDouble d) =
        do withCons fmt (C "MyDouble" (Left 1) False 0) $ withField fmt $
                                                          writeSmart fmt d

instance SmartCopy Double where
    readSmart fmt = do r <- readPrim fmt
                       case r of
                         PrimDouble d -> return d
                         PrimInt i    -> return $ fromIntegral i
                         _ -> fail "Was expecting double primitive, not "
    writeSmart fmt d = writePrimitive fmt (PrimDouble d)
         

instance SmartCopy MyBool where
    readSmart fmt =
        readCons fmt [(C "MyTrue" (Left 0) False 0, return MyTrue), (C "MyFalse" (Left 0) False 1, return MyFalse)]
    writeSmart fmt MyTrue =
        do withCons fmt (C "MyTrue" (Left 0) False 0) $ return ()
    writeSmart fmt MyFalse =
        do withCons fmt (C "MyFalse" (Left 0) False 0) $ return ()

instance SmartCopy Easy where
    readSmart fmt =
        readCons fmt [(C "Easy" (Left 1) False 0, readEasy)]
        where
          readEasy =
              Easy <$> (readField fmt $ readSmart fmt)
    writeSmart fmt (Easy a) =
        do withCons fmt (C "Easy" (Left 1) False 0) $ withField fmt $
                                                      writePrimitive fmt (PrimInt a)

instance SmartCopy Bla where
    readSmart fmt =
        readCons fmt [(C "Bla" (Left 0) False 0, return Bla)]
    writeSmart fmt Bla =
        do withCons fmt (C "Bla" (Left 0) False 0) (return ())

instance SmartCopy Bar where
    writeSmart fmt (BarLeft) =
        do withCons fmt (C "BarLeft" (Left 0) True 0) (return ())
    writeSmart fmt (BarRight foo) =
        do withCons fmt (C "BarRight" (Left 1) True 1) 
               (withField fmt $ writeSmart fmt foo)
    readSmart fmt =
        readCons fmt [ (C "BarLeft" (Left 0) True 0, readBarLeft)
                       , (C "BarRight" (Left 1) True 1, readBarRight)]
        where readBarLeft = return BarLeft
              readBarRight = do f <- readField fmt (readSmart fmt)
                                return $ BarRight f 


instance SmartCopy Some' where
    writeSmart fmt (Some' spam') =
        do withCons fmt (C "Some'" (Left 1) False 0) $ withField fmt (writeSmart fmt spam')
    readSmart fmt =
        readCons fmt [(C "Some'" (Left 1) False 0, readSome')]
        where readSome' = do spam <- readField fmt (readSmart fmt)
                             return $ Some' spam

instance SmartCopy Some where
    writeSmart fmt (Some spam int) =
        do withCons fmt (C "Some" (Left 2) False 0) fields
           where fields = do withField fmt (writeSmart fmt spam)
                             withField fmt (writeSmart fmt int)
    readSmart fmt =
        do readCons fmt [(C "Some" (Left 2) False 0, readSome)]
        where readSome = do spam <- readField fmt (readSmart fmt)
                            int <- readField fmt (readSmart fmt)
                            return $ Some spam int

                             
instance SmartCopy Spam where
    writeSmart fmt (Spam int) =
        do withCons fmt (C "Spam" (Left 1) False 0) $ withField fmt $
                         (writeSmart fmt int)
    readSmart fmt =
        readCons fmt [(C "Spam" (Left 1) False 0, readSpam)]
        where readSpam =
                do i <- readField fmt (readSmart fmt)
                   return $ Spam i

instance SmartCopy Spam' where
    writeSmart fmt (Spam' int1 int2) =
        do withCons fmt (C "Spam'" (Left 2) False 0) writeFields
        where writeFields = 
               do withField fmt (writeSmart fmt int1)
                  withField fmt (writeSmart fmt int2)
    readSmart fmt =
        readCons fmt [(C "Spam'" (Left 2) False 0, readSpam')]
        where readSpam' =
                do i1 <- readField fmt (readSmart fmt)
                   i2 <- readField fmt (readSmart fmt)
                   return $ Spam' i1 i2

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

v1 :: Foo
v1 = Foo 2 BarLeft

v2 :: Foo
v2 = Foo 42 (BarRight (Foo 41 BarLeft))

v7 :: FooBar
v7 = Foo0 MyTrue (MyDouble 42.0) 

v8 :: FooBar
v8 = Bar0 23 v7

v9 :: Bla
v9 = Bla

v10 :: ArrType
v10 = ArrType [1,2,3,4]

v11 :: ArrTypeBar
v11 = ArrTypeBar [BarRight v1, BarLeft]

v12 :: ArrTypeFooBar
v12 = ArrTypeFooBar [v7, v8]

some1 :: Some
some1 = Some (Spam 1) 2

some2 :: Some'
some2 = Some' (Spam' 1 2)

---- Json Values

v3 :: Json.Value
v3 = Json.Number 3

v4 :: Json.Value
v4 = Json.Number 2.3

v5 :: Json.Value
v5 = Json.Object $ M.fromList
                   [ ("tag", Json.String "Foo0")
                   , ("contents", Json.Array $ V.fromList
                   [Json.String "MyTrue", Json.Number 43])]

v6 :: Json.Value
v6 = Json.Object $ M.fromList
                   [ ("tag", Json.String "Bar0")
                   , ("value", Json.Number 2)
                   , ("foobar", v5)
                   ]

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
                   liftIO $ print (J.parseSmart v3 :: Fail Easy)
                   liftIO $ print (J.parseSmart v5 :: Fail FooBar)
                   liftIO $ print (J.parseSmart v6 :: Fail FooBar)
                   putStrLn "DATATYPES as JSON Values:"
                   liftIO $ print (J.serializeSmart v7)
                   liftIO $ print (J.serializeSmart (MyDouble 23))
                   liftIO $ print (J.serializeSmart (Easy 42))
                   liftIO $ print (J.serializeSmart some1)
                   liftIO $ print (J.serializeSmart some2)                   
                   liftIO $ print (J.serializeSmart v1)
                   liftIO $ print (J.serializeSmart v2)
                   liftIO $ print (J.serializeSmart v8)
                   liftIO $ print (J.serializeSmart v10)
                   liftIO $ print (J.serializeSmart v11)
                   liftIO $ print (J.serializeSmart v12)
                   putStrLn "ENCODING:"
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v8))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v7))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v9))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v10))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v11))
                   liftIO $ BSL.putStrLn (J.encode (J.serializeSmart v12))
            "string":_ ->
                do putStrLn "DATATYPES as String Values:"
                   liftIO $ print (S.serializeSmart v8)
                   liftIO $ print (S.serializeSmart (Easy 42))
                   liftIO $ print (S.serializeSmart v2)
                   liftIO $ print (S.serializeSmart (MyDouble 23))
                   liftIO $ print (S.serializeSmart v1)
                   liftIO $ print (S.serializeSmart v7)
                   liftIO $ print (S.serializeSmart v10)
                   liftIO $ print (S.serializeSmart v11)
                   liftIO $ print (S.serializeSmart v12)
                   liftIO $ print (S.serializeSmart some1)
                   liftIO $ print (S.serializeSmart some2)
                   liftIO $ print (S.serializeSmart s6parsed)
                   putStrLn "PARSING String Values:"
                   liftIO $ print (S.parseSmart somestring1 :: Fail Some)
                   liftIO $ print (S.parseSmart somestring2 :: Fail Some')
                   liftIO $ print (S.parseSmart s1 :: Fail Easy)
                   liftIO $ print (S.parseSmart s2 :: Fail FooBar)
                   liftIO $ print (S.parseSmart s3 :: Fail FooBar)
                   liftIO $ print (S.parseSmart s4 :: Fail ArrType)
                   liftIO $ print (S.parseSmart s5 :: Fail ArrTypeBar)
                   liftIO $ print (S.parseSmart s6 :: Fail ArrTypeBar)
                   liftIO $ print (S.parseSmart s7 :: Fail ArrTypeFooBar)
            "xml":_ ->
                do putStrLn "DATATYPES xml-encoded:"
                   liftIO $ putStrLn (X.serializeSmart v9)
                   liftIO $ putStrLn (X.serializeSmart v2)
                   liftIO $ putStrLn (X.serializeSmart (MyDouble 23))
                   liftIO $ putStrLn (X.serializeSmart (Easy 42))
                   liftIO $ putStrLn (X.serializeSmart v1)
                   liftIO $ putStrLn (X.serializeSmart v7)
                   liftIO $ putStrLn (X.serializeSmart v8)
                   liftIO $ putStrLn (X.serializeSmart v10)
                   liftIO $ putStrLn (X.serializeSmart v11)
                   liftIO $ putStrLn (X.serializeSmart v12)
                   liftIO $ putStrLn (X.serializeSmart some1)
                   liftIO $ putStrLn (X.serializeSmart some2)
                   liftIO $ putStrLn (X.serializeSmart s6parsed)
                   putStrLn "PARSING XML:"
                   liftIO $ print (X.parseSmart xml1 :: Fail Easy)
                   liftIO $ print (X.parseSmart xml3 :: Fail Bla)
                   liftIO $ print (X.parseSmart xml4 :: Fail Some)
                   liftIO $ print (X.parseSmart xml2 :: Fail FooBar)
                   liftIO $ print (X.parseSmart xml5 :: Fail Some')
            _  ->
                do putStrLn $ "You need to specify a format to display tested examplesin.\
                              \ Valid formats at this moment are: " ++ show fmtList
                             
                


