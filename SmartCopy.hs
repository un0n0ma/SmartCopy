{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartCopy where

import Control.Monad
import Control.Applicative
import qualified Data.Vector as V
import GHC.Generics

-------------------------------------------------------------------------------
-- General Format types
-------------------------------------------------------------------------------

class SerializedType a where
    getType :: a

class (SerializedType a, Functor m) => FormatMonad m a where
    unPack :: m b -> a
    returnEmpty :: m a
    enterCons :: CT -> m a -> m a
    enterField :: FT -> m a -> m a
    openRepetition :: Int -> [m a] -> m a
    writeValue :: Prim -> m a
    closeRepetition :: m a
    leaveField :: m ()
    leaveCons :: m ()
    mult :: m a -> m a -> m a

data CFVType = CT
             | FT
             | Prim

data CT = Cons Prim Bool -- True for sumtypes
data FT = Field Int String
data Prim = PrimInt Integer
          | PrimChar Char
          | PrimString String 

type IdNum = Int

-------------------------------------------------------------------------------
-- SmartCopy
-------------------------------------------------------------------------------
class (SerializedType b, FormatMonad m b) => SmartCopy a m b | m -> b where
    serialize :: a -> m b
    default serialize :: (Generic a, GSmartCopy (Rep a) m b)
                      => a -> m b
    serialize a = gserialize (from a)
    parse :: m b -> a
    default parse :: (Generic a, GSmartCopy (Rep a) m b)
                  => m b -> a
    parse b = to (gparse b)

class (SerializedType a, FormatMonad m a) => GSmartCopy t m a | m -> a where
    gserialize :: t x -> m a
    gparse :: m a -> (t x)

instance (SerializedType a, FormatMonad m a) => SmartCopy Integer m a where
    serialize i = writeValue (PrimInt i)
    parse b = undefined

instance (SerializedType a, FormatMonad m a) => SmartCopy Int m a where
    serialize i = writeValue (PrimInt $ toInteger i)
    parse b = undefined

instance (SerializedType a, FormatMonad m a) => SmartCopy Char m a where
    serialize c = writeValue (PrimChar c)
    parse b = undefined

instance (SerializedType a, FormatMonad m a) => SmartCopy String m a where
    serialize s = writeValue (PrimString s)
    parse b = undefined

instance (SerializedType b, FormatMonad m b, SmartCopy a m b) => SmartCopy [a] m b where
    serialize [] = closeRepetition
    serialize a@(x:xs) = let n = length a
                         in openRepetition n (map serialize xs)

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance (SerializedType a, FormatMonad m a) => GSmartCopy U1 m a where
    gserialize _ = returnEmpty

instance (SerializedType b, FormatMonad m b, GSmartCopy a m b, Constructor c) =>
            GSmartCopy (C1 c a) m b where
    gserialize m1 = let cons = Cons (PrimString $ conName m1) True
                        inside = gserialize (unM1 m1)
                    in enterCons cons inside

instance (SerializedType b, FormatMonad m b, GSmartCopy a m b, Datatype d) =>
            GSmartCopy (D1 d a) m b where
    gserialize m1 = gserialize (unM1 m1)

instance (SerializedType b, FormatMonad m b, GSmartCopy a m b, Selector s) => GSmartCopy (S1 s a) m b where
    gserialize m1 = let field = (Field 0 (selName m1)) ---- FIX THIS! (Int for field number)
                        inside = gserialize (unM1 m1)
                    in enterField field inside

instance (SerializedType c, GSmartCopy a m c, GSmartCopy b m c, FormatMonad m c) =>
            GSmartCopy (a :+: b) m c where
    gserialize c@(L1 a) = gserialize a
    gserialize c@(R1 a) = gserialize a

instance (SerializedType c, GSmartCopy a m c, GSmartCopy b m c, FormatMonad m c) =>
            GSmartCopy (a :*: b) m c where
    gserialize (a :*: b) = mult (gserialize a) (gserialize b)

instance (FormatMonad m b, SerializedType b, SmartCopy a m b) =>
        GSmartCopy (K1 g a) m b where
    gserialize (K1 a) = serialize a
