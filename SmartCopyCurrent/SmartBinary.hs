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


serializeSmart a = runPut (writeSmart binarySerializationFormat a)

parseSmart :: SmartCopy a => BS.ByteString -> Either String a
parseSmart = runGet (readSmart binaryParseFormat)

binarySerializationFormat :: SerializationFormat PutM
binarySerializationFormat
    = SerializationFormat
    { withCons =
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
    where write = writeSmart binarySerializationFormat
    
binaryParseFormat :: ParseFormat Get
binaryParseFormat
    = ParseFormat
    { readCons =
        \cons ->
            case length cons of
              0 -> fail "No constructor to look up."
              1 -> 
                if ctagged $ fst $ head cons
                   then fail $
                        "Expecting a sumtype, but there is only one constructor for look-up: " ++
                        show $ cname $ fst $ head cons
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
                        show $ map (cname . fst) cons
    , readField = id
    , readRepetition =
          getListOf $ readSmart binaryParseFormat
    , readPrim = 
           readInt --- Clearly not working. Fix!
           <|> readInteger
           <|> readDouble
           <|> readString
           <|> readBool
    }

readInt =
    do prim <- S.getWord64be
       return $ PrimInt $ fromIntegral prim

readString =
    do prim  <- S.getListOf (S.get :: Get Char)
       return $ PrimString prim

readBool =
    do prim <- S.getWord8
       return $ PrimBool $ toEnum $ fromIntegral prim

readChar =
    do prim :: Char <- S.get
       return $ PrimChar prim

readDouble =
    do prim :: Double <- S.get
       return $ PrimDouble prim

readInteger =
    do prim :: Integer <- S.get
       return $ PrimInteger prim
