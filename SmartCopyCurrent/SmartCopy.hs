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
                    PrimDouble d -> return $ floor d
                    PrimInteger i -> return $ fromIntegral i
                    _         -> fail $ "Was expecting int primitive, not " ++ show prim
            
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt i

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

instance SmartCopy Double where
    readSmart fmt = do prim <- readPrim fmt
                       case prim of
                         PrimDouble d -> return d
                         PrimInt i -> return $ realToFrac i
                         PrimInteger i -> return $ realToFrac i
                         _ -> fail $ "Was expecting double primitive, not " ++ show prim
    writeSmart fmt d = writePrimitive fmt (PrimDouble d)


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

instance SmartCopy Char where
    readSmart fmt =
        do prim <- readPrim fmt
           fromPrimChar prim
        where fromPrimChar prim =
                  case prim of
                    PrimChar c -> return c
                    _ -> fail $ "Was expecting char primitive, not " ++ show prim
    writeSmart fmt c =
        writePrimitive fmt $ PrimChar c

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt = undefined -- fix
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b

 
        
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


