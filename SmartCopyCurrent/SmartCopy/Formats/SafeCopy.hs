module SmartCopy.Formats.SafeCopy
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.Instances
import SmartCopy.MonadTypesInstances
import SmartCopy.SmartCopy

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


-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------
serializeSmart a = S.runPut $ smartPut sFormat a

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = S.runGet $ smartGet pFormat

serializeUnvers a = S.runPut $ writeSmart sFormatUnvers a

parseUnvers :: SmartCopy a => BS.ByteString -> Either String a
parseUnvers = S.runGet $ readSmart pFormatUnvers

-------------------------------------------------------------------------------
-- Versioned serialization
-------------------------------------------------------------------------------
sFormat :: SerializationFormat PutM
sFormat
    = SerializationFormat
    { withVersion = const id
    , writeVersion = S.put . unVersion
    , withCons =
          \cons ma ->
              if ctagged cons
              then do S.putWord8 (fromIntegral $ cindex cons)
                      ma
              else ma
    , withField = id
    , writeRepetition =
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
                PrimString s -> writeRepetition sFormat s
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
    
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------
pFormat :: ParseFormat Get
pFormat
    = ParseFormat
    { readVersioned = id
    , readVersion = liftM (Just . Version) S.get
    , readCons =
        \cons ->
          case length cons of
            0 -> noCons
            n | ctagged $ fst $ head cons ->
                    do c <- S.getWord8
                       let conInds = map (fromIntegral . cindex . fst) cons
                           parsers = map snd cons
                       fromMaybe (mismatch ("constructor with index " ++ show c) (show conInds))
                             (lookup c (zip conInds parsers))
              | n == 1 -> snd $ head cons
              | otherwise ->
                       do let conNames = map (cname . fst) cons
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
             if b then liftM Just $ smartGet pFormat
                  else return Nothing
    }

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { writeVersion = \_ -> return () 
    , writeRepetition = putListOf (writeSmart sFormatUnvers)
    , writeString =
          \prim ->
          case prim of
            PrimString s -> writeRepetition sFormatUnvers s
            _ -> mismatch "Prim string" (show prim)
    , writeMaybe =
          \m ->
              case m of
                Just a ->
                    S.put True >> writeSmart sFormatUnvers a
                Nothing ->
                    S.put False
    }

pFormatUnvers
    = pFormat
    { readVersion = return Nothing
    , readRepetition = getListOf (readSmart pFormatUnvers)
    , readString =
          do s <- readRepetition pFormatUnvers
             return $ PrimString s
    , readMaybe =
          do b <- S.get
             if b then liftM Just $ readSmart pFormatUnvers
                  else return Nothing
    }
