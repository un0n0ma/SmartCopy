{-# LANGUAGE PackageImports #-}

module Data.SmartCopy.Formats.SafeCopy
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy
import Data.SmartCopy.SmartCopy
       ( mismatch
       , conLookupErr
       , noCons
       , getSmartGet
       , getSmartPut
       , constructGetterFromVersion
       , ConstrInfo (..)
       )


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

import "mtl" Control.Monad.State

import Control.Applicative
import Data.Maybe
import Data.Either (rights, lefts)
import Data.Typeable


-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------
serializeSmart a = S.runPut $ smartPut sFormat a

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = S.runGet (smartGet pFormat)

serializeUnvers a = S.runPut $ writeSmart sFormatUnvers a

parseUnvers :: SmartCopy a => BS.ByteString -> Either String a
parseUnvers = S.runGet (readSmart pFormatUnvers)

-------------------------------------------------------------------------------
-- Versioned serialization
-------------------------------------------------------------------------------
sFormat :: SerializationFormat PutM
sFormat
    = SerializationFormat
    { mkPutter =
          \b ver -> if b then S.put ver >> return (writeSmart sFormat)
                         else return $ writeSmart sFormat
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
          \b prevVers ->
              if b 
                 then do v <- liftM Version S.get
                         case constructGetterFromVersion pFormat v kind of
                           Right getter -> return getter
                           Left msg -> fail msg
                 else either fail return $
                      constructGetterFromVersion pFormat (Version prevVers) kind
    , readCons =
        \cons ->
          case cons of
            [] -> noCons
            [(CInfo _ _ False _, parser)] ->
                parser
            [(CInfo _ _ True _, parser)] ->
                do let conNames = map (cname . fst) cons
                   mismatch "tagged type" (show conNames)
            (CInfo _ _ True _, _):_ ->
                do let conInds = map (fromIntegral . cindex . fst) cons
                       parsers = map snd cons
                   c <- S.getWord8
                   fromMaybe
                     (mismatch ("constructor with index " ++ show c) (show conInds))
                     (lookup c (zip conInds parsers))
            f -> fail $ show (map fst f)
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
             if b then liftM Just (smartGet pFormat)
                  else return Nothing
    , readBS = S.get
    , readText = liftM decodeUtf8 (smartGet pFormat)
    }

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------

sFormatUnvers
    = sFormat
    { mkPutter = \_ v -> return $ writeSmart sFormatUnvers
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
    { mkGetter = \_ _ -> return $ readSmart pFormatUnvers 
    , readRepetition =
          do n <- S.get
             getSmartGet pFormatUnvers >>= replicateM n
    , readString = readRepetition pFormatUnvers
    , readMaybe =
          do b <- S.get
             if b then liftM Just $ readSmart pFormatUnvers
                  else return Nothing
    }
