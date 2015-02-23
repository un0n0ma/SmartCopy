{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module SmartCopy.Instances where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Generic
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC

import Data.Int (Int32)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Monad (liftM2)


instance SmartCopy a => SmartCopy (SC.Prim a) where
    version = 0
    kind = primitive
    readSmart fmt =
        do res <- readSmart fmt
           return $ SC.Prim res
    writeSmart fmt (SC.Prim a) =
        writeSmart fmt a

instance SmartCopy Int where
    readSmart fmt =
        do prim <- readInt fmt
           fromPrinInt prim
        where fromPrinInt prim =
                  case prim of
                    PrimInt i -> return i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writeInt fmt $ PrimInt i

instance SmartCopy Int32 where
    version = 0
    readSmart fmt =
        do prim <- readInt fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return $ fromIntegral i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writeInt fmt $ PrimInt $ fromIntegral i

instance SmartCopy Char where
    readSmart fmt =
        do prim <- readChar fmt
           fromPrimChar prim
        where fromPrimChar prim =
                  case prim of
                    PrimChar c -> return c
                    f          -> mismatch "Char prim" (show f)
    writeSmart fmt c =
        writeChar fmt $ PrimChar c

instance SmartCopy Double where
    version = 0
    readSmart fmt =
        do prim <- readDouble fmt
           case prim of
             PrimDouble d -> return d
             f            -> mismatch "Double prim" (show f)
    writeSmart fmt d = writeDouble fmt (PrimDouble d)

instance SmartCopy String where
    version = 0
    readSmart fmt =
        do prim <- readString fmt
           fromPrimString prim
        where fromPrimString prim =
                  case prim of
                    PrimString s -> return s
                    f            -> mismatch "String prim" (show f)
    writeSmart fmt s =
        writeString fmt $ PrimString s

instance SmartCopy Bool where
    version = 0
    readSmart fmt =
        do prim <- readBool fmt
           fromPrimBool prim
        where fromPrimBool prim =
                  case prim of
                    PrimBool b -> return b
                    f          -> mismatch "Bool prim" (show f)
    writeSmart fmt b =
        writeBool fmt $ PrimBool b

instance SmartCopy a => SmartCopy (Maybe a) where
    version = 0
    readSmart = readMaybe
    writeSmart = writeMaybe

instance SmartCopy a => SmartCopy [a] where
    version = 0
    readSmart = readRepetition
    writeSmart = writeRepetition

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt = liftM2 (,) (readSmart fmt) (readSmart fmt)
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b
