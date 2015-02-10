{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
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
                    f         -> fail $ "Was expecting int prim at " ++ show f ++ "."
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt i

instance SmartCopy Char where
    readSmart fmt =
        do prim <- readChar fmt
           fromPrimChar prim
        where fromPrimChar prim =
                  case prim of
                    PrimChar c -> return c
                    f         -> fail $ "Was expecting char prim at " ++ show f ++ "."
    writeSmart fmt c =
        writePrimitive fmt $ PrimChar c

{-
instance SmartCopy Integer where
    readSmart fmt =
        do prim <- readPrim fmt
           fromPrimInteger prim
        where fromPrimInteger prim =
                  case prim of
                    PrimInteger i -> return i
                    PrimInt i -> return $ toInteger i
                    _ -> fail $ "Was expecting integer primtive, not " ++ show prim
    writeSmart fmt i =
        writePrimitive fmt $ PrimInteger i
        -}

instance SmartCopy Double where
    readSmart fmt = 
        do prim <- readDouble fmt
           case prim of
             PrimDouble d -> return d
             f         -> fail $ "Was expecting double prim at " ++ show f ++ "."
    writeSmart fmt d = writePrimitive fmt (PrimDouble d)


instance SmartCopy String where
    readSmart fmt =
        do prim <- readString fmt
           fromPrimString prim
        where fromPrimString prim =
                  case prim of
                    PrimString s -> return s
                    f         -> fail $ "Was expecting string prim at " ++ show f ++ "."
    writeSmart fmt s =
        writePrimitive fmt $ PrimString s

instance SmartCopy Bool where
    readSmart fmt =
        do prim <- readBool fmt
           fromPrimBool prim
        where fromPrimBool prim =
                  case prim of
                    PrimBool b -> return b
                    f         -> fail $ "Was expecting double prim at " ++ show f ++ "."
    writeSmart fmt b =
        writePrimitive fmt $ PrimBool b

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt = undefined -- fix
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b

 
        
-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

data SerializationFormat m
    = SerializationFormat
    { withVersion :: forall a. Version a -> m () -> m ()
    , withCons :: Cons -> m () -> m ()
    , withField :: m () -> m ()
    , withRepetition :: SmartCopy a => [a] -> m ()
    , writePrimitive :: Prim -> m ()
    }

data ParseFormat m
    = ParseFormat
    { readCons :: forall a. [(Cons, m a)] -> m a
    , readField :: forall a. m a -> m a
    , readRepetition :: SmartCopy a => m [a]
    , readInt :: m Prim
    , readChar :: m Prim
    , readBool :: m Prim
    , readDouble :: m Prim
    , readString :: m Prim
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
-- Utility functions and types from SafeCopy
-------------------------------------------------------------------------------

newtype Version a = Version { unVersion :: Int32 }

data Kind a where
    Primitive :: Kind a
    Base :: Kind a
-- ... Extends..

