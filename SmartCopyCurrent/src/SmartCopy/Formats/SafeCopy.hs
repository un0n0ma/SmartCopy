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
parseSmart = S.runGet (fromEitherM $ evalStateT (smartGet pFormat) Nothing)

serializeUnvers a = S.runPut $ writeSmart sFormatUnvers a

parseUnvers :: SmartCopy a => BS.ByteString -> Either String a
parseUnvers = S.runGet (fromEitherM $ evalStateT (readSmart pFormatUnvers) Nothing)

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
pFormat :: ParseFormat (StateT (Maybe Word8) Get)
pFormat
    = ParseFormat
    { mkGetter =
          \b prevVers ->
              if b 
                 then do v <- lift $ liftM Version S.get
                         case constructGetterFromVersion pFormat v kind of
                           Right getter -> return getter
                           Left msg -> fail msg
                 else either fail return $
                      constructGetterFromVersion pFormat (Version prevVers) kind
    , withLookahead =
          \conInd ma mb ->
          do prevTag <- get
             case prevTag of
               Nothing ->
                 do c <- lift S.getWord8
                    put $ Just c
                    if c == fromIntegral conInd
                       then ma
                       else mb
               Just c ->
                 if c == fromIntegral conInd
                    then ma
                    else mb
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
                do prevTag <- get
                   let conInds = map (fromIntegral . cindex . fst) cons
                       parsers = map snd cons
                   case prevTag of
                     Nothing ->
                         do c <- lift S.getWord8
                            fromMaybe
                              (mismatch ("constructor with index " ++ show c) (show conInds))
                              (lookup c (zip conInds parsers))
                     Just c ->
                         do put Nothing
                            fromMaybe
                              (mismatch ("constructor with index " ++ show c) (show conInds))
                              (lookup c (zip conInds parsers))
    , readField = id
    , readRepetition =
          do n <- lift S.get
             res  <- getSmartGet pFormat >>= replicateM n
             case lefts res of
               [] ->
                   return $ Right $ rights res
               l:_ ->
                   return $ Left l
    , readInt = lift $ liftM Right S.get
    , readChar = lift $ liftM Right S.get
    , readBool = lift $ liftM (Right . toEnum . fromIntegral) S.getWord8
    , readDouble = lift $ liftM Right S.get
    , readString = readRepetition pFormat
    , readMaybe =
          do b <- lift S.get
             if b then smartGet pFormat >>= either (return . Left) (return . Right . Just)
                  else return $ Right Nothing
    , readBS = lift $ liftM Right S.get
    , readText = smartGet pFormat >>= either (return . Left) (return . Right . decodeUtf8)
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
          do n <- lift S.get
             res  <- getSmartGet pFormatUnvers >>= replicateM n
             case lefts res of
               [] ->
                   return $ Right $ rights res
               l:_ ->
                   return $ Left l
    , readString = readRepetition pFormatUnvers
    , readMaybe =
          do b <- lift S.get
             if b then liftM (fmap Just) $ readSmart pFormatUnvers
                  else return $ Right Nothing
    }
