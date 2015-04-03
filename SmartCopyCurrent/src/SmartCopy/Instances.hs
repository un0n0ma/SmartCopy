{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SmartCopy.Instances where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Generic
import SmartCopy.SmartCopy

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
    version = 0
    kind = primitive
    readSmart fmt =
        do res <- readSmart fmt
           either fail (return . Right . SC.Prim) res
    writeSmart fmt (SC.Prim a) =
        writeSmart fmt a

instance SmartCopy Int where
    readSmart = readInt
    writeSmart = writeInt

instance SmartCopy Int32 where
    readSmart fmt =
        do res <- readInt fmt
           either fail (return . Right . fromIntegral) res
    writeSmart fmt = writeInt fmt . fromIntegral

instance SmartCopy Char where
    readSmart = readChar
    writeSmart = writeChar

instance SmartCopy Double where
    readSmart = readDouble
    writeSmart = writeDouble

instance SmartCopy String where
    readSmart = readString
    writeSmart = writeString

instance SmartCopy Bool where
    readSmart = readBool
    writeSmart = writeBool

instance SmartCopy a => SmartCopy (Maybe a) where
    readSmart = readMaybe
    writeSmart = writeMaybe

instance SmartCopy a => SmartCopy [a] where
    readSmart = readRepetition
    writeSmart = writeRepetition

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt
        = do res1 :: Either String a <- readSmart fmt
             res2 :: Either String b <- readSmart fmt
             case res1 of
               Right a1 ->
                   case res2 of
                     Right a2 -> return $ Right (a1, a2)
                     Left msg -> fail msg
               Left msg -> fail msg
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b

instance SmartCopy BS.ByteString where
    readSmart = readBS
    writeSmart = writeBS

instance SmartCopy T.Text where
    readSmart = readText
    writeSmart = writeText
