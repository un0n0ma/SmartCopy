{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module SmartCopyTest where


import Control.Monad
import Control.Applicative
import GHC.Generics

-------------------------------------------------------------------------------
-- General Format types
-------------------------------------------------------------------------------
data Format m a
    = Format
    { fmt_enterDataCon :: CT -> m a
    , fmt_enterField :: FT ->  m a
    , fmt_writeValue :: Prim -> m a
    , fmt_leaveField :: m a
    , fmt_leaveDataCon :: m a
    , fmt_nothing :: m a
    }

class FormatMonad m where
    type ReturnType :: *
    unPack :: m a -> ReturnType
    liftFM :: (a -> b) -> m a -> m b
    returnCons :: CT -> m a
    returnField :: FT -> m a
    returnValue :: Prim -> m a
    leaveField :: m a
    leaveCons :: m a
    add :: m a -> m b -> m c

data CFVType = CT
             | FT
             | Prim

data CT = Cons String
data FT = Field Int (Maybe String)

data Prim = PrimInt Int | PrimChar Char | PrimString String


{--
class AddType t1 t2 where
    add :: forall (m :: * -> *) b.
           Format m b -> FormatMonad m => t1 -> t2 -> m b

instance AddType CT FT where
    add _ ct ft = (returnCons ct) .>> (returnField ft)
    --}

-------------------------------------------------------------------------------
-- SmartCopy
-------------------------------------------------------------------------------
class SmartCopy a where
    serialize :: FormatMonad m
              => a -> Format m b -> m b
    default serialize :: (FormatMonad m, Generic a, GSmartCopy (Rep a))
                      => a -> Format m b -> m b
    serialize a fmt = gserialize (from a) fmt
    parse :: FormatMonad m => b -> Format m b -> m a
    default parse :: (FormatMonad m, Generic a, GSmartCopy (Rep a))
                  =>  b -> Format m b -> m a
    parse b fmt = undefined
--    parse b fmt = to `liftFM` (gparse b fmt)

class GSmartCopy t where
    gserialize :: (FormatMonad m) => t x -> Format m b -> m b
    gparse :: FormatMonad m => b -> Format m b -> m (t x)


instance SmartCopy Int where
    serialize i fmt = (fmt_writeValue fmt) (PrimInt i)
    parse b = undefined

instance SmartCopy Char where
    serialize c fmt = (fmt_writeValue fmt) (PrimChar c)
    parse b = undefined

instance SmartCopy String where
    serialize s fmt = (fmt_writeValue fmt) (PrimString s)
    parse b = undefined

instance SmartCopy a => SmartCopy [a] where
    serialize [] fmt = fmt_nothing fmt
    serialize (x:xs) fmt = let h = serialize x fmt 
                               t = serialize xs fmt
                           in add h t

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance GSmartCopy U1 where
    gserialize _ fmt = fmt_nothing fmt

instance (GSmartCopy a, Constructor c) => GSmartCopy (C1 c a) where
    gserialize m1 fmt = let cons = (fmt_enterDataCon fmt) (Cons (conName m1))
                            inside = gserialize (unM1 m1) fmt
                        in add cons inside

instance (GSmartCopy a, Datatype d) => GSmartCopy (D1 d a) where
    gserialize m1 fmt = gserialize (unM1 m1) fmt

instance (GSmartCopy a, Selector s) => GSmartCopy (S1 s a) where
    gserialize m1 fmt = let field = (fmt_enterField fmt) (Field 0 (Just $ selName m1)) ---- FIX THIS! (Int)
                            inside = gserialize (unM1 m1) fmt
                        in add field inside

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :+: b) where
    gserialize (L1 a) fmt = gserialize a fmt
    gserialize (R1 a) fmt = gserialize a fmt

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gserialize (a :*: b) fmt = add (gserialize a fmt) (gserialize b fmt)

instance SmartCopy a => GSmartCopy (K1 g a) where
    gserialize (K1 a) fmt = serialize a fmt
