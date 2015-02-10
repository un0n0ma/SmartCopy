{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module SmartCopy where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S

import Data.Int
import Data.Text.Internal as T

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (MonadTrans(..))
import "mtl" Control.Monad.Writer

class SmartCopy a where
    version :: Version a
    version = Version 0
    kind :: Kind a
    kind = Base
    writeSmart :: (SmartCopy a, Monad m) => SerializationFormat m -> a -> m ()
    readSmart :: (Functor m, Applicative m, Monad m) => ParseFormat m -> m a

instance SmartCopy Int where
    readSmart fmt =
        do prim <- readInt fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt i

instance SmartCopy Int32 where
    readSmart fmt =
        do prim <- readInt fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return $ fromIntegral i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt $ fromIntegral i

instance SmartCopy Char where
    readSmart fmt =
        do prim <- readChar fmt
           fromPrimChar prim
        where fromPrimChar prim =
                  case prim of
                    PrimChar c -> return c
                    f         -> mismatch "Char prim" (show f)
    writeSmart fmt c =
        writePrimitive fmt $ PrimChar c

instance SmartCopy Double where
    readSmart fmt = 
        do prim <- readDouble fmt
           case prim of
             PrimDouble d -> return d
             f         -> mismatch "Double prim" (show f)
    writeSmart fmt d = writePrimitive fmt (PrimDouble d)


instance SmartCopy String where
    readSmart fmt =
        do prim <- readString fmt
           fromPrimString prim
        where fromPrimString prim =
                  case prim of
                    PrimString s -> return s
                    f         -> mismatch "String prim" (show f)
    writeSmart fmt s =
        writePrimitive fmt $ PrimString s

instance SmartCopy Bool where
    readSmart fmt =
        do prim <- readBool fmt
           fromPrimBool prim
        where fromPrimBool prim =
                  case prim of
                    PrimBool b -> return b
                    f         -> mismatch "Bool prim" (show f)
    writeSmart fmt b =
        writePrimitive fmt $ PrimBool b

instance SmartCopy a => SmartCopy (Maybe a) where
    readSmart = readMaybe
    writeSmart = writeMaybe

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt = liftM2 (,) (readSmart fmt) (readSmart fmt)
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b

instance SmartCopy a => SmartCopy (Version a) where
    readSmart fmt = readVersion fmt
    writeSmart fmt = writeVersion fmt

instance S.Serialize (Version a) where
    get = liftM Version S.get
    put = S.put . unVersion

instance Num (Version a) where
    Version a + Version b = Version (a+b)
    Version a * Version b = Version (a*b)
    Version a - Version b = Version (a-b)
    negate (Version a) = Version (negate a)
    abs (Version a) = Version (abs a)
    signum (Version a) = Version (signum a)
    fromInteger i = Version (fromInteger i)

-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

data SerializationFormat m
    = SerializationFormat
    { writeVersion :: forall a. Version a -> m ()
    , withCons :: Cons -> m () -> m ()
    , withField :: m () -> m ()
    , withRepetition :: SmartCopy a => [a] -> m ()
    , writePrimitive :: Prim -> m ()
    , writeMaybe :: SmartCopy a => Maybe a -> m ()
    }

data ParseFormat m
    = ParseFormat
    { readVersion :: forall a. m (Version a)
    , readCons :: forall a. [(Cons, m a)] -> m a
    , readField :: forall a. m a -> m a
    , readRepetition :: SmartCopy a => m [a]
    , readInt :: m Prim
    , readChar :: m Prim
    , readBool :: m Prim
    , readDouble :: m Prim
    , readString :: m Prim
    , readMaybe :: SmartCopy a => m (Maybe a)
    }


-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

mismatch :: Monad m => forall a. String -> String -> m a
mismatch exp act = fail $ "Was expecting " ++ exp ++ " at " ++ act ++ "."

noCons :: Monad m => forall a. m a
noCons = fail "No constructor found during look-up."

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Cons
    = C
    { cname :: T.Text
    , cfields :: Fields
    , ctagged :: Bool
    , cindex :: Int
    }

data Fields = NF Int
            | LF [Label]
            | Empty --- Empty is for types where no constructor has fields (differently represented in JSON than Cons .. 0 .. ..)

type Label = T.Text

data Prim = PrimInt Int
          | PrimInteger Integer
          | PrimChar Char
          | PrimString String
          | PrimBool Bool
          | PrimDouble Double
          deriving (Show, Read)


-------------------------------------------------------------------------------
-- Version control
-------------------------------------------------------------------------------

-- When implementing a format that deals with versioned data,
-- those are the functions to be used in serializeSmart and parseSmart.
-- In unversioned formats the functions can directly run readSmart and writeSmart.

smartPut :: (SmartCopy a, Monad m) => SerializationFormat m -> a ->  m ()
smartPut fmt a =
    do putter <- getSmartPut fmt
       putter a

smartGet :: (SmartCopy a, Monad m, Applicative m)
         => ParseFormat m
         -> m a
smartGet fmt = join $ getSmartGet fmt

-- Same as SmartCopy but dealing with arbitrary monads.

getSmartPut :: forall a m. (SmartCopy a, Monad m)
           => SerializationFormat m
           -> m (a -> m ())
getSmartPut fmt =
    case kindFromProxy proxy of
      Primitive -> return $ \a -> writeSmart fmt $ asProxyType a proxy
      _         -> do writeSmart fmt (versionFromProxy proxy)
                      return $ \a -> writeSmart fmt $ asProxyType a proxy
    where proxy = Proxy :: Proxy a

getSmartGet :: forall a m. (SmartCopy a, Monad m, Applicative m)
           => ParseFormat m
           -> m (m a)
getSmartGet fmt =
    case kindFromProxy proxy of
      Primitive -> return $ readSmart fmt
      kind -> do v <- readSmart fmt :: m (Version a)
                 case constructGetterFromVersion fmt v kind of
                   Right getter -> return getter
                   Left msg -> fail msg
      where proxy = Proxy :: Proxy a


-- Migrate class

class SmartCopy (MigrateFrom a) => Migrate a where
    type MigrateFrom a
    migrate :: MigrateFrom a -> a

-- Types and utility functions from SafeCopy (SafeCopy exports are not sufficient)

newtype Version a = Version { unVersion :: Int32 } deriving (Eq, Show)

castVersion :: Version a -> Version b
castVersion (Version a) = Version a

data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extends   :: Migrate a => Proxy (MigrateFrom a) -> Kind a
    -- TODO: Add Extended

extension :: (SmartCopy a, Migrate a) => Kind a
extension = Extends Proxy

base :: Kind a
base = Base

primitive :: Kind a
primitive = Primitive

data Proxy a = Proxy

versionFromProxy :: SmartCopy a => Proxy a -> Version a
versionFromProxy _ = version

kindFromProxy :: SmartCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a

constructGetterFromVersion :: forall a m. (SmartCopy a, Monad m, Applicative m, Functor m)
                           => ParseFormat m
                           -> Version a
                           -> Kind a
                           -> Either String (m a)
constructGetterFromVersion fmt diskV origK =
    worker fmt False diskV origK
    where
    worker :: forall a m. (SmartCopy a, Monad m, Applicative m, Functor m)
           => ParseFormat m
           -> Bool
           -> Version a
           -> Kind a
           -> Either String (m a)
    worker fmt fwd thisV thisK
        | version == thisV = return $ readSmart fmt
        | otherwise =
          case thisK of
          --- */ TODO: add detailed error messages
            Primitive -> Left "Cannot migrate from primitive types."
            Base -> Left "Version not found"
            Extends bProxy ->
                do previousGetter <- worker fmt fwd (castVersion diskV) (kindFromProxy bProxy)
                   return $ fmap migrate previousGetter
                   -- TODO: add Extended
