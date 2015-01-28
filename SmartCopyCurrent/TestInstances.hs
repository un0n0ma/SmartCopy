{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestInstances where

import "mtl" Control.Monad.Reader hiding (sequence)

import Control.Applicative
import Control.Monad
import GHC.Generics

import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------
import MonadTypesInstances
import JSON
import SmartCopy


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


----------------------
-- SmartCopy instances
----------------------

instance SmartCopy Foo where
    readSmart fmt =
      do readCustom fmt [(C "Foo" (Left 2) False 0, readFoo)]
      where readFoo =
                 do int <- readField fmt (Left 0) $ readNum fmt
                    bar <- readField fmt (Left 1) $ readSmart fmt
                    return $ Foo int bar

    writeSmart fmt (Foo i bar) =
        do beginWritingCons fmt (C "Foo" (Left 2) False 0)
           withField fmt (Left 0) (writePrimitive fmt $ PrimInt i)
           withField fmt (Left 1) (writeSmart fmt bar)

instance SmartCopy FooBar where
    readSmart fmt =
      do readCustom fmt [(C "Foo0" (Left 2) True 0, readFoo0),
                         ((C "Bar0" (Right ["value", "foobar"])  True 1), readFoo1)]
      where
        readFoo0 =
               do myBool <- readField fmt (Left 0) $ readSmart fmt
                  myDouble <- readField fmt (Left 1) $ readSmart fmt
                  return $ Foo0 myBool myDouble
        readFoo1 = 
               do val <- readField fmt (Right "value") $ readNum fmt
                  foobar <- readField fmt (Right "foobar") $ readSmart fmt
                  return $ Bar0 val foobar

    writeSmart fmt (Foo0 bool double) =
        do beginWritingCons fmt (C "Foo0" (Left 2) True 0) 
           withField fmt (Left 0) (writeSmart fmt bool)
           withField fmt (Left 1) (writeSmart fmt double)
    writeSmart fmt (Bar0 int foobar) =
        do beginWritingCons fmt (C "Bar0" (Right ["value", "foobar"]) True 1)
           withField fmt (Right "value") (writePrimitive fmt (PrimInt int))
           withField fmt (Right "foobar") (writeSmart fmt foobar)


instance SmartCopy MyDouble where
    readSmart fmt =
        do readCustom fmt [((C "MyDouble" (Left 1) False 0), readMyDouble)]
           where readMyDouble = do d <- readField fmt (Left 0) $ readSmart fmt
                                   return $ MyDouble d
    writeSmart fmt (MyDouble d) =
        do beginWritingCons fmt (C "MyDouble" (Left 1) False 0)
           writePrimitive fmt (PrimInt $ floor d)

instance SmartCopy Double where
    readSmart fmt = do r <- readNum fmt
                       return $ realToFrac r
    writeSmart fmt d = writePrimitive fmt (PrimInt $ floor d)
         

instance SmartCopy MyBool where
    readSmart fmt =
        readCustom fmt [(C "MyTrue" (Left 0) False 0, return MyTrue), (C "MyFalse" (Left 0) False 1, return MyFalse)]
    writeSmart fmt MyTrue =
        do beginWritingCons fmt (C "MyTrue" (Left 0) False 0)
    writeSmart fmt MyFalse =
        do beginWritingCons fmt (C "MyFalse" (Left 0) False 0)

instance SmartCopy Easy where
    readSmart fmt =
        readCustom fmt [(C "Easy" (Left 1) False 0, readEasy)]
        where
          readEasy =
              Easy <$> (readField fmt (Left 0) $ readNum fmt)
    writeSmart fmt (Easy a) =
        do beginWritingCons fmt (C "Easy" (Left 1) False 0)
           writePrimitive fmt (PrimInt a)

instance SmartCopy Bla where
    readSmart fmt =
        readCustom fmt [(C "Bla" (Left 0) False 0, return Bla)]
    writeSmart fmt Bla =
        do beginWritingCons fmt (C "Bla" (Left 0) False 0)

instance SmartCopy Bar where
    writeSmart fmt (BarLeft) =
        do beginWritingCons fmt (C "BarLeft" (Left 0) True 0)
    writeSmart fmt (BarRight foo) =
        do beginWritingCons fmt (C "BarRight" (Left 1) True 1)
           withField fmt (Left 0) $ writeSmart fmt foo
    readSmart fmt =
        readCustom fmt [ (C "BarLeft" (Left 0) True 0, readBarLeft)
                       , (C "BarRight" (Left 1) True 1, readBarRight)]
        where readBarLeft = return BarLeft
              readBarRight = do f <- readField fmt (Left 0) (readSmart fmt)
                                return $ BarRight f 


instance SmartCopy Some' where
    writeSmart fmt (Some' spam') =
        do beginWritingCons fmt (C "Some'" (Left 1) False 0)
           withField fmt (Left 0) (writeSmart fmt spam')
    readSmart fmt =
        readCustom fmt [(C "Some'" (Left 1) False 0, readSome')]
        where readSome' = do spam <- readField fmt (Left 0) (readSmart fmt)
                             return $ Some' spam

instance SmartCopy Some where
    writeSmart fmt (Some spam int) =
        do beginWritingCons fmt (C "Some" (Left 2) False 0)
           withField fmt (Left 0) (writeSmart fmt spam)
           withField fmt (Left 1) (writePrimitive fmt $ PrimInt int)
    readSmart fmt =
        do readCustom fmt [(C "Some" (Left 2) False 0, readSome)]
        where readSome = do spam <- readField fmt (Left 0) (readSmart fmt)
                            int <- readField fmt (Left 1) (readNum fmt)
                            return $ Some spam int

                             
instance SmartCopy Spam where
    writeSmart fmt (Spam int) =
        do beginWritingCons fmt (C "Spam" (Left 1) False 0)
           writePrimitive fmt (PrimInt int)
    readSmart fmt =
        readCustom fmt [(C "Spam" (Left 1) False 0, readSpam)]
        where readSpam =
                do i <- readField fmt (Left 0) (readNum fmt)
                   return $ Spam i

instance SmartCopy Spam' where
    writeSmart fmt (Spam' int1 int2) =
        do beginWritingCons fmt (C "Spam'" (Left 2) False 0)
           withField fmt (Left 0) (writePrimitive fmt (PrimInt int1))
           withField fmt (Left 1) (writePrimitive fmt (PrimInt int2))
    readSmart fmt =
        readCustom fmt [(C "Spam'" (Left 2) False 0, readSpam')]
        where readSpam' =
                do i1 <- readField fmt (Left 0) (readNum fmt)
                   i2 <- readField fmt (Left 1) (readNum fmt)
                   return $ Spam' i1 i2


v1 :: Foo
v1 = Foo 2 BarLeft

v2 :: Foo
v2 = Foo 42 (BarRight (Foo 41 BarLeft))

v3 :: Json.Value
v3 = Json.Number 3

v4 :: Json.Value
v4 = Json.Number 23

v5 :: Json.Value
v5 = Json.Object $ M.fromList
                   [ ("tag", Json.String "Foo0")
                   , ("contents", Json.Array $ V.fromList
                   [Json.String "MyTrue", Json.Number 43])]

v6 :: Json.Value
v6 = Json.Object $ M.fromList
                   [ ("tag", Json.String "Bar0")
                   , ("contents", Json.Object $ M.fromList
                     [("value", Json.Number 2), ("foobar", v5)])]

v7 :: FooBar
v7 = Foo0 MyTrue (MyDouble 42) 

v8 :: FooBar
v8 = Bar0 23 v7

v9 :: Bla
v9 = Bla

some1 :: Some
some1 = Some (Spam 1) 2


some2 :: Some'
some2 = Some' (Spam' 1 2)

main = do putStrLn "TESTING MAPPEND:"
          liftIO $ print (serializeSmart jsonSerializationFormat some1)
          liftIO $ print (serializeSmart jsonSerializationFormat some2)
          putStrLn "PARSING JSON Values:"
          liftIO $ print (parseSmart jsonParseFormat v3 :: Fail Easy)
          liftIO $ print (parseSmart jsonParseFormat v5 :: Fail FooBar)
          liftIO $ print (parseSmart jsonParseFormat v6 :: Fail FooBar)
          putStrLn "DATATYPES as JSON Values:"
          liftIO $ print (serializeSmart jsonSerializationFormat (MyDouble 23))
          liftIO $ print (serializeSmart jsonSerializationFormat (Easy 42))
          liftIO $ print (serializeSmart jsonSerializationFormat v1)
          liftIO $ print (serializeSmart jsonSerializationFormat v2)
          liftIO $ print (serializeSmart jsonSerializationFormat v7)
          liftIO $ print (serializeSmart jsonSerializationFormat v8)
          putStrLn "ENCODING:"
          liftIO $ BSL.putStrLn (encode (serializeSmart jsonSerializationFormat v8))
          liftIO $ BSL.putStrLn (encode (serializeSmart jsonSerializationFormat v7))
          liftIO $ BSL.putStrLn (encode (serializeSmart jsonSerializationFormat v9))


