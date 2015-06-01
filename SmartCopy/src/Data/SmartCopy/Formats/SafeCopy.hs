{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |Formats for serialization and deserialization versioned binary compatible
-- with Data.SafeCopy and unversioned binary compatible with Data.Serialize.
module Data.SmartCopy.Formats.SafeCopy
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       , serializeLastKnown
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy
import Data.SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Bits
import Data.Char (ord)
import Data.Int
import Data.List (unfoldr)
import Data.Serialize.Put
import Data.Serialize.Get
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word8
import Data.Word

import qualified Data.Serialize as S

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.State

import Control.Applicative
import Data.Maybe
import Data.Either (rights, lefts)
import Data.Typeable

import qualified Data.ByteString as BS

-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------
-- |Convert a datatype made an instance of SmartCopy into a versioned
-- ByteString. Compatible to Data.SafeCopy.
serializeSmart :: SmartCopy a => a -> BS.ByteString
serializeSmart a = S.runPut $ smartPut sFormat a

-- |Parse a datatype made an instance of SmartCopy from a versioned ByteString.
-- Compatible to Data.SafeCopy.
parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = S.runGet (smartGet pFormat)

-- |Convert a datatype made an instance of SmartCopy into an unversioned
-- ByteString. Compatible to Data.Serialize.
serializeUnvers :: SmartCopy a => a -> BS.ByteString
serializeUnvers a = S.runPut $ writeSmart sFormatUnvers a Nothing

-- |Parse a datatype made an instance of SmartCopy from an unversioned
-- ByteString. Compatible to Data.Serialize.
parseUnvers :: SmartCopy a => BS.ByteString -> Either String a
parseUnvers = S.runGet (readSmart pFormatUnvers)

-- |Check if a datatype version is known by a communicating component,
-- indicated by its identifier being present in the list of all known
-- identifiers. Convert the latest known version of the datatype into a 
-- versioned ByteString.
serializeLastKnown :: SmartCopy a => a -> [String] -> BS.ByteString
serializeLastKnown a ids = S.runPut $ smartPutLastKnown sFormatBackComp a ids

-------------------------------------------------------------------------------
-- Versioned serialization with back-compatibility
-------------------------------------------------------------------------------
sFormatBackComp :: SerializationFormat PutM
sFormatBackComp
    = sFormat
    { mkPutter =
          \b ver mids ->
              case mids of
                Just _ ->
                    if b then S.put ver >> return (\a -> writeSmart sFormatBackComp a mids)
                         else return $ \a -> writeSmart sFormatBackComp a mids
                Nothing ->
                    fail $ noIDListErr "[type not yet known]"
    , writeRepetition =
          \lst mids ->
              case mids of
                Just allIds ->
                    do S.put (length lst)
                       getSmartPutLastKnown sFormatBackComp allIds >>= forM_ lst
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => [a]"
    , writeMaybe =
          \m mids ->
              case mids of
                Just allIds ->
                    case m of
                      Just a ->
                          S.put True >> smartPutLastKnown sFormatBackComp a allIds
                      Nothing ->
                          S.put False
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => Maybe a"
    }

-------------------------------------------------------------------------------
-- Versioned serialization
-------------------------------------------------------------------------------
sFormat :: SerializationFormat PutM
sFormat
    = SerializationFormat
    { mkPutter =
          \b ver _ -> if b then S.put ver >> return (\a -> writeSmart sFormat a Nothing)
                           else return $ \a -> writeSmart sFormat a Nothing
    , withCons =
          \cons ma ->
              if ctagged cons
              then do S.putWord8 (fromIntegral $ cindex cons)
                      ma
              else ma
    , withField = id
    , writeRepetition =
          \lst _ ->
              do S.put (length lst)
                 getSmartPut sFormat >>= forM_ lst
    , writeInt = S.put
    , writeChar = S.put
    , writeInteger = S.put
    , writeString = \s -> writeRepetition sFormat s Nothing
    , writeBool = S.put
    , writeDouble = S.put
    , writeMaybe =
          \m _ ->
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
          \b dupVers ->
              if b 
                 then do v <- liftM Version S.get
                         case constructGetterFromVersion pFormat v kind of
                           Right getter -> return getter
                           Left msg -> fail msg
                 else either fail return $
                      constructGetterFromVersion pFormat (Version dupVers) kind
    , readCons =
        \cons ->
          case cons of
            [] -> noCons
            [(CInfo _ _ False _ _, parser)] ->
                parser
            [(CInfo _ _ True _ _, parser)] ->
                do let conNames = map (cname . fst) cons
                   mismatch "tagged type" (show conNames)
            (CInfo _ _ True _ _, _):_ ->
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
    { mkPutter = \_ v _ -> return $ \a -> writeSmart sFormatUnvers a Nothing
    , writeRepetition =
          \rep _ -> putListOf (\a -> writeSmart sFormatUnvers a Nothing) rep
    , writeString = \s -> writeRepetition sFormatUnvers s Nothing
    , writeMaybe =
          \m _ ->
              case m of
                Just a ->
                    S.put True >> writeSmart sFormatUnvers a Nothing
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
