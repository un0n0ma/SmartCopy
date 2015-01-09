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
-- Useful SafeCopy types
-------------------------------------------------------------------------------
data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extension :: (Migrate a) => Proxy (MigrateFrom a) -> Kind a

data Version a = Version Int

class Migrate a where
    type MigrateFrom a
    migrate :: MigrateFrom a -> a

data Proxy a = Proxy

-------------------------------------------------------------------------------
-- General Format types
-------------------------------------------------------------------------------

class Functor m => FormatMonad m where
    type EncodedType :: *
    unPack :: m a -> EncodedType
    returnEmpty :: m EncodedType
    enterCons :: CT -> m EncodedType -> m EncodedType
    enterField :: FT -> m EncodedType -> m EncodedType
    openRepetition :: Int -> [m EncodedType] -> m EncodedType
    writeValue :: Prim -> m EncodedType
    closeRepetition :: m EncodedType
    leaveField :: m ()
    leaveCons :: m ()
    mult :: m EncodedType -> m EncodedType -> m EncodedType

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
class FormatMonad m => SmartCopy a m where
    version :: Version a
    version = Version 0
    kind :: Kind a
    kind = Base
    serialize :: a -> m EncodedType
    default serialize :: (Generic a, GSmartCopy (Rep a) m)
                      => a -> m EncodedType
    serialize a = gserialize (from a)
    parse :: m EncodedType -> Maybe a
    default parse :: (Generic a, GSmartCopy (Rep a) m)
                  => m EncodedType -> Maybe a
    parse b = to (gparse b)

class FormatMonad m => GSmartCopy t m where
    gserialize :: t x -> m EncodedType
    gparse :: m EncodedType -> (t (Maybe x))

instance FormatMonad m => SmartCopy Integer m where
    serialize i = writeValue (PrimInt i)
    parse b = undefined

instance FormatMonad m => SmartCopy Int m where
    serialize i = writeValue (PrimInt $ toInteger i)
    parse b = undefined

instance FormatMonad m => SmartCopy Char m where
    serialize c = writeValue (PrimChar c)
    parse b = undefined

instance FormatMonad m => SmartCopy String m where
    serialize s = writeValue (PrimString s)
    parse b = undefined

instance (FormatMonad m, SmartCopy a m) => SmartCopy [a] m where
    serialize [] = closeRepetition
    serialize a@(x:xs) = let n = length a
                         in openRepetition n (map serialize xs)

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance FormatMonad m => GSmartCopy U1 m where
    gserialize _ = returnEmpty

instance (FormatMonad m, GSmartCopy a m, Constructor c) => GSmartCopy (C1 c a) m where
    gserialize m1 = let cons = Cons (PrimString $ conName m1) True
                        inside = gserialize (unM1 m1)
                    in enterCons cons inside

instance (FormatMonad m, GSmartCopy a m, Datatype d) => GSmartCopy (D1 d a) m where
    gserialize m1 = gserialize (unM1 m1)

instance (FormatMonad m, GSmartCopy a m, Selector s) => GSmartCopy (S1 s a) m where
    gserialize m1 = let field = (Field 0 (selName m1)) ---- FIX THIS! (Int for field number)
                        inside = gserialize (unM1 m1)
                    in enterField field inside

instance (GSmartCopy a m, GSmartCopy b m, FormatMonad m) => GSmartCopy (a :+: b) m where
    gserialize c@(L1 a) = gserialize a
    gserialize c@(R1 a) = gserialize a

instance (GSmartCopy a m, GSmartCopy b m, FormatMonad m) => GSmartCopy (a :*: b) m where
    gserialize (a :*: b) = mult (gserialize a) (gserialize b)

instance SmartCopy a m => GSmartCopy (K1 g a) m where
    gserialize (K1 a) = serialize a
