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
import MonadTypesInstances


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
        writePrimitive fmt $ PrimInt i
        
data SerializationFormat m r
    = SerializationFormat
    { runSerialization :: m () -> r
    , beginWritingCons :: Cons -> m ()
    , withField :: Either Int LabeledField -> m () -> m ()
    , writePrimitive :: Prim -> m ()
    , endWritingCons :: m ()
    }

data ParseFormat i m
    = ParseFormat
    { runParser :: SmartCopy a => m a -> i -> Fail a
    , readCustom :: SmartCopy a => [(Cons, m a)] -> m a
    , readField :: SmartCopy a => Either Int LabeledField -> m a -> m a
    , readNum :: m Int
    , readBool :: m Bool
    }



-------------------------------------------------------------------------------
--Other
-------------------------------------------------------------------------------

data Cons
    = C
    { cname :: T.Text
    , cfields :: Either Int [LabeledField]
    , ctagged :: Bool
    , cindex :: Int
    }

type LabeledField = T.Text

data Prim = PrimInt Int
          | PrimString String
          | PrimBool Bool


