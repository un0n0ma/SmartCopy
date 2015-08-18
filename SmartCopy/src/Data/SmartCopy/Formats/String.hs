{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |Formats for serializing and deserializing a datatype in a simple String
-- representation, enclosing constructor and field names and parenthesises.
-- Unversioned or additionally writing out all version tags after the
-- constructor name deriveSafeCopy-style.
module Data.SmartCopy.Formats.String
       ( serializeUnvers
       , parseUnvers
       , serializeSmart
       , parseSmart
       , serializeLastKnown
       , parseLastKnown
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy
import Data.SmartCopy.MonadTypesInstances (FailT, runFailT)
import Data.SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.List.Utils (startswith)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L (span)
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Monad.Identity
import "mtl" Control.Monad.State
import "mtl" Control.Monad.Writer

-------------------------------------------------------------------------------
-- Run functions, versioned and unversioned
-------------------------------------------------------------------------------

-- |Convert a datatype made an instance of SmartCopy into its versioned String
-- representation. The String format handles versions in the same way as
-- deriveSafeCopy, so that the version tags of all fields of a datatype are
-- written out in the beginning.
serializeSmart :: SmartCopy a => a -> String
serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = snd $ runWriter m

-- |Parse a datatype made an instance of SmartCopy from its versioned String
-- representation.
parseSmart :: SmartCopy a => String -> Fail a
parseSmart = runParser (smartGet pFormat)

-- |Convert a datatype made an instance of SmartCopy into its unversioned String
-- representation.
serializeUnvers :: SmartCopy a => a -> String
serializeUnvers a = runSerialization (writeSmart sFormatUnvers a Nothing)
    where runSerialization m = snd $ runWriter m

-- |Parse a datatype made an instance of SmartCopy from its unversioned String
-- representation.
parseUnvers :: SmartCopy a => String -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action = evalState (runFailT action)

-- |Check if a datatype version is known by a communicating component,
-- indicated by its identifier being present in the list of all known
-- identifiers. Convert the latest known version of the datatype into its
-- versioned String representation.
serializeLastKnown :: SmartCopy a => a -> [String] -> String
serializeLastKnown a ids =
    runSerialization $ smartPutLastKnown sFormatBackComp a ids
    where runSerialization m = snd $ runWriter m

-- |Parse a versioned datatype serialized using the back-compatible String
-- format.
parseLastKnown :: SmartCopy a => String -> Fail a
parseLastKnown =
    runParser (smartGet pFormatBackComp)

runParser :: forall s a. FailT (StateT s Identity) a -> s -> Fail a
runParser action = evalState (runFailT action)

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------
sFormatUnvers :: SerializationFormat (Writer String)
sFormatUnvers
    = sFormat
    { mkPutter = \_ _ _ -> return $ \a -> writeSmart sFormatUnvers a Nothing
    , writeRepetition =
          \rep _ ->
              do tell "["
                 case length rep of
                   0 -> return ()
                   _ -> do mapM_ (\a ->
                                  do writeSmart sFormatUnvers a Nothing
                                     tell ",") (init rep)
                           writeSmart sFormatUnvers (last rep) Nothing
                 tell "]"
    , writeMaybe =
          \ma _ ->
              case ma of
                Nothing -> tell "Nothing"
                Just a -> writeSmart sFormatUnvers a Nothing
    }

-------------------------------------------------------------------------------
-- Unversioned parsing
-------------------------------------------------------------------------------

pFormatUnvers :: ParseFormat (FailT (State String))
pFormatUnvers
    = pFormat
    { readRepetition =
          do str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/= ']') xs
                      case rest of
                        ']':xs -> do
                            put list
                            res <- mapWithDelim (readSmart pFormatUnvers) list []
                            put xs
                            return res
                        _ -> mismatch "]" rest
               _      -> mismatch "[" str'
    , readMaybe =
          do str' <- get
             let str = filter (/=' ') str'
             if startswith "Nothing" str
                then do put $ snd $ delimit str
                        return Nothing
                else liftM Just $ readSmart pFormatUnvers
    }

-------------------------------------------------------------------------------
-- Versioned serialization with back-migration
-------------------------------------------------------------------------------

sFormatBackComp :: SerializationFormat (Writer String)
sFormatBackComp
    = sFormat
    { mkPutter =
          \b ver mIds ->
              case mIds of
                Just _ ->
                    if b
                       then do wrapM $ tell $ "version:" ++ show ver
                               return $ \a -> writeSmart sFormatBackComp a mIds
                       else return $ \a -> writeSmart sFormatBackComp a mIds
                Nothing ->
                    fail $ noIDListErr "[type not yet known]"
    , withCons =
          \cons ma ->
              do { tell $ cidentifier cons ++ ":" ++ show (cindex cons); ma }
    , writeRepetition =
          \rep mIds ->
              case mIds of
                Just allIds ->
                    do tell "["
                       case rep of
                         [] -> return ()
                         (x:xs) ->
                             do putter <- getSmartPutLastKnown sFormatBackComp allIds
                                putter x
                                tell ","
                                mapM_ (\a ->
                                          do writeSmart sFormatBackComp a mIds
                                             tell ",") $ init xs
                                writeSmart sFormatBackComp (last xs) mIds
                       tell "]"
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => [a]"
    , writeMaybe =
          \ma mIds ->
              case mIds of
                Just allIds ->
                    case ma of
                      Nothing -> tell "Nothing"
                      Just a -> smartPutLastKnown sFormatBackComp a allIds
                Nothing ->
                    fail $ noIDListErr "SmartCopy a => Maybe a"
     }
    where wrapM m = do { tell " ("; _ <- m; tell ")" }

-------------------------------------------------------------------------------
-- Versioned parsing with back-migration
-------------------------------------------------------------------------------

pFormatBackComp :: ParseFormat (FailT (State String))
pFormatBackComp
    = pFormat
    { mkGetter =
          \b dupVers ->
              if b
                 then
                     do let kind = kindFromProxy (Proxy :: Proxy a)
                        version <- readVersion
                        case version of
                          Just v ->
                              case constructGetterFromVersion pFormatBackComp v kind of
                                      Right getter -> return getter
                                      Left msg -> fail msg
                          Nothing -> return $ readSmart pFormatBackComp
                 else either fail return $
                      constructGetterFromVersion pFormatBackComp (Version dupVers) kind
    , readCons =
          \cons ->
              case cons of
                [] -> noCons
                x:_ ->
                   do con <- startCons
                      let conId = cidentifier (fst x) ++ ":"
                          conLkp = map ((++) conId . show . cindex . fst) cons
                          parsers = map snd cons
                      case lookup con (zip conLkp parsers) of
                        Just parser ->
                           parser
                        _ -> conLookupErr (show con) (show conLkp)
    , readRepetition =
          do _ <- readVersion
             str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/=']') xs
                      case rest of
                        ']':xs' ->
                            do put list
                               getter <- getSmartGet pFormatBackComp
                               res <- mapWithDelim getter list []
                               put xs'
                               return res
                        _ -> mismatch "]" rest
               _ -> mismatch "[" str'
    , readMaybe =
          do str' <- get
             let str = filter (/=' ') str'
             if startswith "Nothing" str
                then do put $ snd $ delimit str
                        return Nothing
                else liftM Just $ smartGet pFormatBackComp
    }
-------------------------------------------------------------------------------
-- Versioned serialization
-------------------------------------------------------------------------------

sFormat :: SerializationFormat (Writer String)
sFormat
    = SerializationFormat
    { mkPutter =
          \b ver _ ->
              if b
                 then do wrapM $ tell $ "version:" ++ show ver
                         return $ \a -> writeSmart sFormat a Nothing
                 else return $ \a -> writeSmart sFormat a Nothing
    , withCons =
          \cons ma ->
              do { tell $ T.unpack $ cname cons; ma }
    , withField =
          wrapM
    , writeRepetition =
          \rep _ ->
              do tell "["
                 case rep of
                   [] -> return ()
                   (x:xs) ->
                       do putter <- getSmartPut sFormat
                          putter x
                          tell ","
                          mapM_ (\a ->
                                    do writeSmart sFormat a Nothing
                                       tell ",") $ init xs
                          writeSmart sFormat (last xs) Nothing
                 tell "]"
    , writeInt = tell . show
    , writeInteger = tell . show
    , writeChar = tell . show
    , writeDouble = tell . show
    , writeString = tell
    , writeBool = tell . show
    , writeMaybe =
          \ma _ ->
              case ma of
                Nothing -> tell "Nothing"
                Just a -> smartPut sFormat a
    , writeBS = tell . BSC.unpack
    , writeText = tell . T.unpack
    }
    where wrapM m = do { tell " ("; _ <- m; tell ")" }

-------------------------------------------------------------------------------
-- Versioned parsing
-------------------------------------------------------------------------------

pFormat :: ParseFormat (FailT (State String))
pFormat
    = ParseFormat
    { mkGetter =
          \b dupVers ->
              if b
                 then
                     do let kind = kindFromProxy (Proxy :: Proxy a)
                        version <- readVersion
                        case version of
                          Just v ->
                              case constructGetterFromVersion pFormat v kind of
                                      Right getter -> return getter
                                      Left msg -> fail msg
                          Nothing -> return $ readSmart pFormat
                 else either fail return $
                      constructGetterFromVersion pFormat (Version dupVers) kind
    , readCons =
          \cons ->
              do let conNames = map (cname . fst) cons
                     parsers = map snd cons
                 case cons of
                   [] -> noCons
                   _ ->
                      do con <- startCons
                         case lookup (T.pack con) (zip conNames parsers) of
                           Just parser ->
                              parser
                           _ -> conLookupErr (show con) (show conNames)
    , readField =
          \ma ->
              do _ <- readOpen
                 res <- ma
                 _ <- readClose
                 return res
    , readRepetition =
          do _ <- readVersion
             str' <- get
             let str = filter (/=' ') str'
             case str of
               '[':xs ->
                   do let (list, rest) = L.span (/=']') xs
                      case rest of
                        ']':xs' ->
                            do put list
                               getter <- getSmartGet pFormat
                               res <- mapWithDelim getter list []
                               put xs'
                               return res
                        _ -> mismatch "]" rest
               _ -> mismatch "[" str'
    , readInt =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return num
               _ -> mismatch "Int" prim
    , readChar =
          do str <- get
             let prim = filter (/=' ') str
             case prim of
               [] ->
                   mismatch "Char" prim
               h:t ->
                   do put t
                      return h
    , readBool =
          do str <- get
             let prim = filter (/=' ') str
             readBool' prim
    , readDouble =
          do str <- get
             let prim = filter (/=' ') str
             case reads prim of
               [(num, xs)] ->
                   do put xs
                      return num
               _ -> mismatch "Double" prim
    , readString =
          do str <- get
             let (prim, rest) = delimit $ filter (/=' ') str
             put rest
             return prim
    , readMaybe =
          do str' <- get
             let str = filter (/=' ') str'
             if startswith "Nothing" str
                then do put $ snd $ delimit str
                        return Nothing
                else liftM Just $ smartGet pFormat
    , readBS =
         do str <- get
            let (prim, rest) = delimit $ filter (/=' ') str
            put rest
            return $ BSC.pack prim
    , readText =
         do str <- get
            let (prim, rest) = delimit $ filter (/=' ') str
            put rest
            return $ T.pack prim
    }
    where readBool' prim
              | startswith "True" prim =
                do put $ drop 4 prim; return True
              | startswith "False" prim =
                do put $ drop 5 prim; return False
              | otherwise =
                mismatch "Bool" prim


-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

startCons :: FailT (State String) String
startCons =
    do str' <- get
       let str = filter (/=' ') str'
       if hasNested str
          then do let (cons, after) = L.span (/='(') str
                  put after
                  return cons
          else do let (cons, after) = L.span (/=')') str
                  put after
                  return cons

readOpen :: FailT (State String) String
readOpen =
    do str' <- get
       let str = filter (/=' ') str'
       case str of
         '(':rest ->
             do put rest
                return ""
         _ ->
            fail $ "No opening parenthesis found at " ++ str ++ "."

hasNested :: [Char] -> Bool
hasNested str = let (untilClosedPar, _) = L.span (/=')') str
                    (untilOpenPar, _) = L.span (/='(') str
                in length untilClosedPar >= length untilOpenPar

readClose :: FailT (State String) String
readClose =
    do str <- get
       case filter (/=' ') str of
         ')':xs ->
             do put xs
                return ""
         _ -> fail $ "No closing parenthesis found at " ++ str ++ "."

mapWithDelim :: forall (m :: * -> *) t. MonadState [Char] m
             => m t -> [Char] -> [t] -> m [t]
mapWithDelim mb list acc
    | null list
    = return []
    | otherwise
    = do let (listelem, listrest) = L.span (/= ',') list
         case T.unpack $ T.strip $ T.pack listrest of
           ',':xs -> do put listelem
                        parseElem <- mb
                        mapWithDelim mb xs (acc ++ [parseElem])
           _ -> do put listelem
                   parseElem <- mb
                   return $ acc ++ [parseElem]

readVersion :: forall a. FailT (State String) (Maybe (Version a))
readVersion =
    do str' <- get
       let str = filter (/=' ') str'
           (untilVer, after) = L.span (/=':') str
       case after of
         ':':xs ->
              case reads xs of
                [(int, ')':afterVer)] ->
                    do let withoutVer = take (length untilVer - 8) untilVer ++ afterVer
                       put withoutVer
                       return $ Just $ Version int
                _ -> mismatch "int32" after
         _ -> return Nothing

delimit :: [Char] -> ([Char], [Char])
delimit = L.span (/=')')
