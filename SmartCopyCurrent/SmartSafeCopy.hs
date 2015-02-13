module SmartSafeCopy where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances
import SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
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


sFormat = safeCopySerializationFormat
pFormat = safeCopyParseFormat

serializeSmart a = S.runPut $ smartPut sFormat a

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = S.runGet $ smartGet pFormat

safeCopySerializationFormat :: SerializationFormat PutM
safeCopySerializationFormat
    = SerializationFormat
    { withVersion =
          \ver ma ->
              do writeVersion sFormat ver  -- version is written after tag! 
                 ma
    , writeVersion = S.put . unVersion
    , withCons =
          \cons ma ->
              if ctagged cons
              then do putWord8 (fromIntegral $ cindex cons)
                      ma
              else ma
    , withField = id
    , withRepetition =
          \lst ->
              do S.put (length lst)
                 getSmartPut sFormat >>= forM_ lst
    , writeInt =
          \prim ->
              case prim of
                PrimInt i ->
                    S.put i
                _ -> mismatch "Prim int" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar i ->
                    S.put i
                _ -> mismatch "Prim char" (show prim)
    , writeInteger =
          \prim ->
              case prim of
                PrimInteger i ->
                    S.put i
                _ -> mismatch "Prim integer" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s -> withRepetition sFormat s
                _ -> mismatch "Prim string" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b ->
                    S.put b
                _ -> mismatch "Prim bool" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d ->
                    S.put d
                _ -> mismatch "Prim double" (show prim)
    , writeMaybe =
          \m ->
              case m of
                Just a ->
                    S.put True >> smartPut sFormat a
                Nothing ->
                    S.put False
    }
    
safeCopyParseFormat :: ParseFormat Get
safeCopyParseFormat
    = ParseFormat
    { readVersioned = id
    , readVersion = liftM Version S.get
    , readCons =
        \cons ->
          case length cons of
            0 -> noCons
            n -> 
                if ctagged $ fst $ head cons
                   then do c <- getWord8
                           let conInds = map (fromIntegral . cindex . fst) cons
                               parsers = map snd cons
                           fromMaybe (mismatch ("constructor with index " ++ show c) (show conInds))
                                     (lookup c (zip conInds parsers))
                   else if n == 1
                           then snd $ head cons
                           else do let conNames = map (cname . fst) cons
                                   mismatch "tagged type" (show conNames)
    , readField = id
    , readRepetition =
          do n <- S.get
             getSmartGet pFormat >>= replicateM n
    , readInt =
          do prim <- S.get
             return $ PrimInt prim
    , readChar =
          do prim <- S.get
             return $ PrimChar prim
    , readBool =
          do prim <- S.getWord8
             return $ PrimBool $ toEnum $ fromIntegral prim
    , readDouble =
          do prim <- S.get
             return $ PrimDouble prim
    , readString =
          do prim <- S.getListOf (S.get :: Get Char)
             return $ PrimString prim
    , readMaybe =
          do b <- S.get
             if b then liftM Just $ smartGet safeCopyParseFormat
                  else return Nothing
    }
