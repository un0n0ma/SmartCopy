module SmartSafeCopy where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances
import SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as Safe
import qualified Data.Serialize as S

import Data.Bits
import Data.Char (ord)
import Data.Int
import Data.List (unfoldr)
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Word8
import Data.Word

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe


serializeSmart a = runPut (writeSmart safeCopySerializationFormat a)

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = runGet (readSmart safeCopyParseFormat)

safeCopySerializationFormat :: SerializationFormat PutM
safeCopySerializationFormat
    = SerializationFormat
    { withVersion = undefined
    , withCons =
          \cons ma ->
          if ctagged cons
             then do putWord8 (fromIntegral $ cindex cons)
                     ma
             else ma
    , withField = id
    , withRepetition = putListOf write
    , writePrimitive =
          \prim ->
              case prim of
                PrimInt i ->
                    putWord64be (fromIntegral i :: Word64)
                PrimInteger i ->
                    S.put i
                PrimString s ->
                    putListOf write s
                PrimBool b ->
                    putWord8 $ fromIntegral $ fromEnum b
                PrimDouble d ->
                    S.put (decodeFloat d)
                PrimChar c ->
                    S.put c
    }
    where write c = writeSmart safeCopySerializationFormat c
    
safeCopyParseFormat :: ParseFormat Get
safeCopyParseFormat
    = undefined
