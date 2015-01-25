{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}


module SmartCopy where

import Control.Applicative
import "mtl" Control.Monad.Identity
import Control.Monad.IO.Class
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.Trans (MonadTrans(..))
import Data.Text.Internal as T

-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------
import Instances


class SmartCopy a where
--    version :: Version a
    writeSmart :: (SmartCopy a, Monad m) => SerializationFormat m r -> a -> m ()
    readSmart :: (Functor m, Applicative m, Monad m) => ParseFormat i m -> m a


serializeSmart :: (Monad m, SmartCopy a)
               => SerializationFormat m r -> a -> r
serializeSmart fmt a = (runSerialization fmt) (writeSmart fmt a)

parseSmart :: (Functor m, Monad m, Applicative m, SmartCopy a)
           => ParseFormat i m -> i -> Fail a
parseSmart fmt = (runParser fmt) (readSmart fmt)


instance SmartCopy Int where
    readSmart fmt =
        fmap fromIntegral $ readNum fmt
    writeSmart fmt i =
        writePrimitive fmt $ PrimNum i
        
data SerializationFormat m r
    = SerializationFormat
    { runSerialization :: m () -> r
    , beginWritingCons :: Cons -> m ()
    , writePrimitive :: Prim -> m ()
    , endWritingCons :: m ()
    }

data ParseFormat i m
    = ParseFormat
    { runParser :: SmartCopy a => m a -> i -> Fail a
    , readCustom :: SmartCopy a => [(Cons, m a)] -> m a
    , readField :: SmartCopy a => Int -> [(Cons, m a)] -> m a
    , readNum :: Integral a => m a
    , readBool :: m Bool
    }



-------------------------------------------------------------------------------
--Other
-------------------------------------------------------------------------------

type Cons = (T.Text, (Bool, Bool)) -- Bool-Args: IsTagged, IsRecord

data Prim = forall a. Integral a => PrimNum a
          | PrimString String
          | PrimBool Bool


