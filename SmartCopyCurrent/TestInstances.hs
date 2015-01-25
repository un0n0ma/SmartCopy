{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import "mtl" Control.Monad.Reader
import qualified Data.Aeson as Json
import Data.HashMap.Strict as M

-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------
import Instances
import JSON
import SmartCopy

data Foo = Foo Int Bar deriving Show
data Bar = BarLeft | BarRight Foo deriving Show
data Easy = Easy Int deriving Show
data FooBar = Foo0 MyBool MyDouble | Bar0 { value :: Int, foobar :: FooBar } deriving Show
data MyDouble = MyDouble Double deriving Show
data MyBool = MyFalse | MyTrue deriving Show

instance SmartCopy FooBar where
    readSmart fmt =
      do readCustom fmt [(("Foo0", (True, True)), readFoo0), (("Bar0", (True, True)), readFoo1)]
      where
        readFoo0 =
               do myBool <- readField fmt 1 [(("MyTrue", (True, False)), return MyTrue),
                                             (("MyFalse", (True, False)),  return MyFalse)]
                  let readMyDouble = do i:: Double <- fmap fromIntegral $ readNum fmt
                                        return $ MyDouble i
                  myDouble <- readField fmt 0 [(("MyDouble", (False, False)), readMyDouble)]
                  return $ Foo0 myBool myDouble
        readFoo1 =
               do 
                  val <- readField fmt 1 [(("value", (False, False)), readNum fmt)]
                  foobar <- readField fmt 0 [(("foobar", (True, False)), do {f :: FooBar <- readSmart fmt; return f })]
                  return $ Bar0 val foobar
    writeSmart fmt (Foo0 bool double) =
        do beginWritingCons fmt ("Foo0", (True, False))
           writeSmart fmt bool
           writeSmart fmt double
    writeSmart fmt (Bar0 int foobar) =
        do beginWritingCons fmt ("Foo1", (True, False))
           writePrimitive fmt (PrimNum int)
           writeSmart fmt foobar


instance SmartCopy MyDouble where
    readSmart fmt =
        do readCustom fmt [(("MyDouble", (False, False)), readMyDouble)]
           where readMyDouble = do i :: Double <- fmap fromIntegral $ readNum fmt
                                   return $ MyDouble i
    writeSmart fmt (MyDouble d) =
        do beginWritingCons fmt ("MyDouble", (False, False))
           writePrimitive fmt (PrimNum $ floor d)
         

instance SmartCopy MyBool where
    readSmart fmt =
        readCustom fmt [(("MyTrue", (True, False)), return MyTrue), (("MyFalse", (True, False)), return MyFalse)]
    writeSmart fmt MyTrue =
        do beginWritingCons fmt ("MyTrue", (True, False))

instance SmartCopy Easy where
    readSmart fmt =
        readCustom fmt [(("Easy", (False, False)), readEasy)]
        where
          readEasy =
              Easy <$> readNum fmt--(apply fmt) (getAt fmt 0) (readInt fmt)
    writeSmart fmt (Easy a) =
        do beginWritingCons fmt ("Easy", (False, False))
           writePrimitive fmt (PrimNum a)

instance SmartCopy Foo where
    writeSmart fmt (Foo i bar) =
        do beginWritingCons fmt ("Foo", (False, False))
           writePrimitive fmt $ PrimNum i
           writeSmart fmt bar

instance SmartCopy Bar where
    writeSmart fmt (BarLeft) =
        do beginWritingCons fmt ("BarLeft", (True, False))
    writeSmart fmt (BarRight foo) =
        do beginWritingCons fmt ("BarRight", (True, False))
           writeSmart fmt foo

v1 :: Foo
v1 = Foo 2 BarLeft

v2 :: Foo
v2 = Foo 42 (BarRight (Foo 41 BarLeft))

v3 :: Json.Value
v3 = Json.Object $ M.fromList [("Easy", Json.Number 3)]

v4 :: Json.Value
v4 = Json.Object $ M.fromList [("MyDouble", Json.Number 2.3)]

v5 :: Json.Value
v5 = Json.Object $ M.fromList
                   [("Foo0", Json.Object $ M.fromList [("MyDouble", Json.Number 4.3), ("MyTrue", Json.Bool True)])]

v6 :: Json.Value
v6 = Json.Object $ M.fromList
                   [("Bar0", Json.Object $ M.fromList 
                   [("value", Json.Number 2),
                    ("foobar", v5)])]

v7 :: FooBar
v7 = Foo0 MyTrue (MyDouble 42) 

v8 :: FooBar
v8 = Bar0 23 v7

main = do liftIO $ print (parseSmart jsonParseFormat v3 :: Fail Easy)
          liftIO $ print (parseSmart jsonParseFormat v5 :: Fail FooBar)
          liftIO $ print (parseSmart jsonParseFormat v6 :: Fail FooBar)
          liftIO $ print (serializeSmart jsonSerializationFormat (MyDouble 2.3))
          liftIO $ print (serializeSmart jsonSerializationFormat (Easy 42))
          liftIO $ print (serializeSmart jsonSerializationFormat v1)
          liftIO $ print (serializeSmart jsonSerializationFormat v2)
          liftIO $ print (serializeSmart jsonSerializationFormat v7)
          liftIO $ print (serializeSmart jsonSerializationFormat v8)
          liftIO $ print (encode (serializeSmart jsonSerializationFormat v8))

