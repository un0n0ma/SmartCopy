{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
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
class Functor m => FormatMonad m where
    type ReturnType :: *
    type ConsType :: *
    type SelType :: *
    type RepType :: *
    type ProdType :: *
    unPack :: m a -> ReturnType
    liftFM :: (ReturnType -> b) -> m ReturnType -> m b
    returnEmpty :: m ReturnType
    enterCons :: CT -> m ReturnType -> m ReturnType
    enterField :: FT -> m ReturnType -> m ReturnType
    openRepetition :: Int -> [m ReturnType] -> m ReturnType
    writeValue :: Prim -> m ReturnType
    closeRepetition :: m ReturnType
    leaveField :: m ()
    leaveCons :: m ()
    mult :: m ReturnType -> m ReturnType -> m ReturnType

data CFVType = CT
             | FT
             | Prim
             | Rpt

data CT = Cons Prim Bool -- True for sumtypes
data FT = Field Int String

data Prim = PrimInt Integer
          | PrimChar Char
          | PrimString String 

type IdNum = Int

-------------------------------------------------------------------------------
-- SmartCopy
-------------------------------------------------------------------------------
class SmartCopy a where
    serialize :: FormatMonad m
              => a -> m ReturnType
    default serialize :: (FormatMonad m, Generic a, GSmartCopy (Rep a))
                      => a -> m ReturnType
    serialize a = gserialize (from a)
    parse :: FormatMonad m => ReturnType -> m a
    default parse :: (FormatMonad m, Generic a, GSmartCopy (Rep a))
                  =>  ReturnType -> m a
    parse b = undefined
--    parse b fmt = to `liftFM` (gparse b fmt)

class GSmartCopy t where
    gserialize :: (FormatMonad m) => t x -> m ReturnType
    gparse :: FormatMonad m => ReturnType -> m (t x)

instance SmartCopy Integer where
    serialize i = writeValue (PrimInt i)
    parse b = undefined

instance SmartCopy Int where
    serialize i = writeValue (PrimInt $ toInteger i)
    parse b = undefined

instance SmartCopy Char where
    serialize c = writeValue (PrimChar c)
    parse b = undefined

instance SmartCopy String where
    serialize s = writeValue (PrimString s)
    parse b = undefined

instance (SmartCopy a) => SmartCopy [a] where
    serialize [] = closeRepetition
    serialize a@(x:xs) = let n = length a
                         in openRepetition n (map serialize xs)

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance GSmartCopy U1 where
    gserialize _ = returnEmpty

instance (GSmartCopy a, Constructor c) => GSmartCopy (C1 c a) where
    gserialize m1 = let cons = Cons (PrimString $ conName m1) True
                        inside = gserialize (unM1 m1)
                    in enterCons cons inside

instance (GSmartCopy a, Datatype d) => GSmartCopy (D1 d a) where
    gserialize m1 = gserialize (unM1 m1)

instance (GSmartCopy a, Selector s) => GSmartCopy (S1 s a) where
    gserialize m1 = let field = (Field 0 (selName m1)) ---- FIX THIS! (Int for field number)
                        inside = gserialize (unM1 m1)
                    in enterField field inside

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :+: b) where
    gserialize c@(L1 a) = gserialize a
    gserialize c@(R1 a) = gserialize a

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gserialize (a :*: b) = mult (gserialize a) (gserialize b)

instance SmartCopy a => GSmartCopy (K1 g a) where
    gserialize (K1 a) = serialize a
