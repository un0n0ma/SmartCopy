{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SmartCopy.Instances where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy.Generic
import Data.SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.SafeCopy as SC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Int (Int32)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad (liftM2, liftM)

instance SmartCopy a => SmartCopy (SC.Prim a) where
    identifier = ID "Data.SafeCopy.Prim"
    version = 0
    kind = primitive
    readSmart fmt = liftM SC.Prim $ readSmart fmt
    writeSmart fmt (SC.Prim a) = writeSmart fmt a

instance SmartCopy Int where
    identifier = ID "Int"
    readSmart = readInt
    writeSmart fmt i _ = writeInt fmt i

instance SmartCopy Int32 where
    identifier = ID "Int32"
    readSmart fmt = liftM fromIntegral $ readInt fmt
    writeSmart fmt i _ = writeInt fmt $ fromIntegral i

instance SmartCopy Char where
    identifier = ID "Char"
    readSmart = readChar
    writeSmart fmt c _ = writeChar fmt c

instance SmartCopy Double where
    identifier = ID "Double"
    readSmart = readDouble
    writeSmart fmt d _ = writeDouble fmt d

instance SmartCopy String where
    identifier = ID "String"
    readSmart = readString
    writeSmart fmt s _ = writeString fmt s

instance SmartCopy Bool where
    identifier = ID "Bool"
    readSmart = readBool
    writeSmart fmt b _ = writeBool fmt b

instance SmartCopy a => SmartCopy (Maybe a) where
    identifier = ID "Maybe"
    readSmart = readMaybe
    writeSmart = writeMaybe

instance SmartCopy a => SmartCopy [a] where
    identifier = ID "List"
    readSmart = readRepetition
    writeSmart = writeRepetition

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    identifier = ID "Tuple"
    readSmart fmt = liftM2 (,) (readSmart fmt) (readSmart fmt)
    writeSmart fmt (a, b) allIds = writeSmart fmt a allIds >> writeSmart fmt b allIds

instance SmartCopy BS.ByteString where
    identifier = ID "Data.ByteString"
    readSmart = readBS
    writeSmart fmt bs _ = writeBS fmt bs

instance SmartCopy T.Text where
    identifier = ID "Data.Text"
    readSmart = readText
    writeSmart fmt t _ = writeText fmt t
