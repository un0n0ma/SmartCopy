{-# LANGUAGE ScopedTypeVariables #-}

module SmartBinary where

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


serializeSmart a = runPut (writeSmart binarySerializationFormat a)

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = runGet (readSmart binaryParseFormat)

binarySerializationFormat :: SerializationFormat PutM
binarySerializationFormat
    = SerializationFormat
    { writeVersion = undefined
    , withCons =
          \cons ma ->
          if ctagged cons
             then do putWord8 (fromIntegral $ cindex cons)
                     ma
             else ma
    , withField = id
    , withRepetition = putListOf write
    , writeInt =
          \prim ->
              case prim of
                PrimInt i ->
                    putWord64be (fromIntegral i :: Word64)
                _ -> mismatch "Prim Int" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s ->
                    putListOf write s
                _ -> mismatch "Prim String" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b ->
                    putWord8 $ fromIntegral $ fromEnum b
                _ -> mismatch "Prim Bool" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d ->
                    S.put (decodeFloat d)
                _ -> mismatch "Prim Double" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar c ->
                    S.put c
                _ -> mismatch "Prim Char" (show prim)
    }
    where write c = writeSmart binarySerializationFormat c
    
binaryParseFormat :: ParseFormat Get
binaryParseFormat
    = ParseFormat
    { readCons =
        \cons ->
            case length cons of
              0 -> noCons
              1 -> 
                if ctagged $ fst $ head cons
                   then fail $
                        "Expecting a sumtype, but there is only one constructor for look-up: " ++
                        show (cname $ fst $ head cons)
                   else snd $ head cons
              n ->
                if ctagged $ fst $ head cons
                   then do 
                           c <- getWord8
                           let conInds = map (fromIntegral . cindex . fst) cons
                               parsers = map snd cons
                           fromMaybe (fail $ "Didn't find constructor with index " ++ show c ++ ".") (lookup c (zip conInds parsers))
                   else fail $
                        "Got more than one constructor for a non-tagged type: " ++
                        show (map (cname . fst) cons)
    , readField = id
    , readRepetition =
          getListOf $ readSmart binaryParseFormat
    , readInt = 
          do prim <- S.getWord64be
             return $ PrimInt $ fromIntegral prim
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
          do prim  <- S.getListOf (S.get :: Get Char)
             return $ PrimString prim
    }
