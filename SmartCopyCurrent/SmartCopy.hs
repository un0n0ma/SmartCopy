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
--    version :: Version a
    writeSmart :: (SmartCopy a, Monad m) => SerializationFormat m -> a -> m ()
    readSmart :: (Functor m, Applicative m, Monad m) => ParseFormat m -> m a


{-
serializeSmart :: (Monad m, SmartCopy a)
               => SerializationFormat m r -> a -> r
serializeSmart fmt a = (runSerialization fmt) (writeSmart fmt a)

parseSmart :: (Functor m, Monad m, Applicative m, SmartCopy a)
           => ParseFormat i m -> i -> Fail a
parseSmart fmt = (runParser fmt) (readSmart fmt)
-}


instance SmartCopy Int where
    readSmart fmt =
        do prim <- readPrim fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return i
                    PrimDouble i -> return $ floor i
                    _         -> fail $ "Was expecting int primitive, not " ++ show prim
            
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt i
        
data SerializationFormat m
    = SerializationFormat
    { withCons :: Cons -> m () -> m ()
    , withField :: Field -> m () -> m ()
    , withRepetition :: forall a. (a -> m ()) -> [a] -> m ()
    , writePrimitive :: Prim -> m ()
    }

data ParseFormat m
    = ParseFormat
    { readCons :: forall a. [(Cons, m a)] -> m a
    , readField :: forall a. Field -> m a -> m a
    , readRepetition :: forall a. m a -> m [a]
    , readPrim :: m Prim
    }



-------------------------------------------------------------------------------
--Other
-------------------------------------------------------------------------------

data Cons
    = C
    { cname :: T.Text
    , cfields :: Either Int [Label]
    , ctagged :: Bool
    , cindex :: Int
    }

data Field = Index Int | Labeled Label

type Label = T.Text

data Prim = PrimInt Int
          | PrimString String
          | PrimBool Bool
          | PrimDouble Double
          deriving (Show, Read)


