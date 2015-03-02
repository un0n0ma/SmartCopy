{-# LANGUAGE PackageImports #-}

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
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word8
import Data.Word

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS

import "mtl" Control.Monad.Reader

import Control.Applicative
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
    { mkPutter =
          \ver -> S.put (unVersion ver) >> return (writeSmart sFormat)
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
    , writeInt = S.put
    , writeChar = S.put
    , writeInteger = S.put
    , writeString = writeRepetition sFormat
    , writeBool = S.put
    , writeDouble = S.put
    , writeMaybe =
          \m ->
              case m of
                Just a ->
                    S.put True >> smartPut sFormat a
                Nothing ->
                    S.put False
    , writeBS = S.put
    , writeText = smartPut sFormat . encodeUtf8
    }
    
-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------
pFormat :: ParseFormat Get
pFormat
    = ParseFormat
    { mkGetter =
          do v <- liftM Version S.get
             let kind = kindFromProxy (Proxy :: Proxy a)
             case constructGetterFromVersion pFormat v kind of
               Right getter -> return getter
               Left msg -> fail msg
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
    , readInt = S.get
    , readChar = S.get
    , readBool = liftM (toEnum . fromIntegral) S.getWord8
    , readDouble = S.get
    , readString = readRepetition pFormat
    , readMaybe =
          do b <- S.get
             if b then liftM Just $ smartGet pFormat
                  else return Nothing
    , readBS = S.get
    , readText = liftM decodeUtf8 (smartGet pFormat)
    }

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { mkPutter = \_ -> return $ writeSmart sFormatUnvers 
    , writeRepetition = putListOf (writeSmart sFormatUnvers)
    , writeString = writeRepetition sFormatUnvers
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
    { mkGetter = return $ readSmart pFormatUnvers 
    , readRepetition = getListOf (readSmart pFormatUnvers)
    , readString = readRepetition pFormatUnvers
    , readMaybe =
          do b <- S.get
             if b then liftM Just $ readSmart pFormatUnvers
                  else return Nothing
    }
