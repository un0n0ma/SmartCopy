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


instance SmartCopy String where
    readSmart fmt =
        do prim <- readPrim fmt
           fromPrimString prim
        where fromPrimString prim =
                  case prim of
                    PrimString s -> return s
                    _ -> fail $ "Was expecting string primitive, not " ++ show prim
    writeSmart fmt s =
        writePrimitive fmt $ PrimString s


instance SmartCopy Bool where
    readSmart fmt =
        do prim <- readPrim fmt
           fromPrimBool prim
        where fromPrimBool prim =
                  case prim of
                    PrimBool b -> return b
                    _ -> fail $ "Was expecting bool primitive, not " ++ show prim
    writeSmart fmt b =
        writePrimitive fmt $ PrimBool b
        
-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

data SerializationFormat m
    = SerializationFormat
    { withCons :: Cons -> m () -> m ()
    , withField :: m () -> m ()
    , withRepetition :: SmartCopy a => [a] -> m ()
    , writePrimitive :: Prim -> m ()
    }

data ParseFormat m
    = ParseFormat
    { readCons :: forall a. [(Cons, m a)] -> m a
    , readField :: forall a. m a -> m a
    , readRepetition :: SmartCopy a => m [a]
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

type Label = T.Text

data Prim = PrimInt Int
          | PrimString String
          | PrimBool Bool
          | PrimDouble Double
          deriving (Show, Read)


