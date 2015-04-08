{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module Data.SmartCopy.Formats.JSON
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       , serializeWith
       , encodeUnvers
       , encodeSmart
       , decodeUnvers
       , decodeSmart
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy
import Data.SmartCopy.MonadTypesInstances
       ( FailT
       , runFailT
       )
import Data.SmartCopy.SmartCopy
       ( ConstrInfo (..)
       , Fields (..)
       , conLookupErr
       , mismatch
       , noCons
       , getSmartGet
       , constructGetterFromVersion
       )

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------

import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Utils (fromFloatDigits)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Aeson as Json (Value(..), object, decode)
import qualified Data.Aeson.Types as JT (Pair)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.State

import Control.Applicative
import Control.Arrow (first)
import Data.Maybe
import Data.Either (rights, lefts, either)


decodeUnvers :: SmartCopy a => LBS.ByteString -> Fail a
decodeUnvers bs = maybe (Fail $ decodeErr bs) parseUnvers (Json.decode bs :: Maybe Json.Value)

decodeSmart :: SmartCopy a => LBS.ByteString -> Fail a
decodeSmart bs = maybe (Fail $ decodeErr bs) parseSmart (Json.decode bs :: Maybe Json.Value)

decodeErr bs = "Failed while decoding ByteString " ++ show bs ++ " into Json Value."

encodeUnvers :: SmartCopy a => a -> LBS.ByteString
encodeUnvers = encodeUtf8 . toLazyText . encodeToTextBuilder . serializeUnvers

encodeSmart :: SmartCopy a => a -> LBS.ByteString
encodeSmart = encodeUtf8 . toLazyText . encodeToTextBuilder . serializeSmart

-------------------------------------------------------------------------------
--  Run functions, versioned and unversioned
-------------------------------------------------------------------------------

serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseSmart :: SmartCopy a => Json.Value -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action value =
              evalState (evalStateT (runReaderT (runFailT action) value) value) []

serializeUnvers a = runSerialization (writeSmart sFormatUnvers a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseUnvers :: SmartCopy a => Json.Value -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action value =
              evalState (runReaderT (runFailT action) value) []

serializeWith a version = runSerialization (smartPutWithVersion sFormat a version)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

-------------------------------------------------------------------------------
--  Versioned serialization
-------------------------------------------------------------------------------

sFormat
    = sFormatUnvers
    { mkPutter =
          \_ ver ->
              return $ \a ->
                  do writeSmart sFormat a
                     res <- lift get
                     let versObj = [("version", Json.Number $ fromIntegral ver)]
                         resObj = case lookup (T.pack "version") (fromObject res) of
                                    Just _ -> versObj ++ [("object", res)]
                                    Nothing -> versObj ++ fromObject res
                     lift $ put $ Json.object resObj
    , withCons =
          \cons ma ->
              if ctagged cons
                 then case cfields cons of
                        Empty ->
                          lift $ put $ Json.String $ cname cons
                        NF 0 ->
                          lift $ put $ Json.object [("tag", Json.String $ cname cons),
                                                    ("contents", Json.Array V.empty)]
                        NF i ->
                          do put $ Left $ Json.Array V.empty
                             _ <- ma
                             Left res <- get
                             let resObj = Json.object [("tag", Json.String $ cname cons),
                                                       ("contents", res)]
                             lift $ put resObj
                        LF ls ->
                          do let fields = zip ls (repeat Json.Null)
                                 fieldsWithInd = zip [0..] $ map (Json.object . return) fields
                             put $ Right $ map (first (T.pack . show)) fieldsWithInd
                             _ <- ma
                             Right res <- get
                             let resObj 
                                  = Json.object $ ("tag", Json.String $ cname cons):res
                             lift $ put resObj
                 else case cfields cons of
                       LF ls ->
                         do let fields = zip ls (repeat Json.Null)
                                fieldsWithInd = zip [0..] $ map (Json.object . return) fields
                            put $ Right $ map (first (T.pack . show)) fieldsWithInd
                            _ <- ma
                            Right res <- get
                            lift $ put $ Json.object res
                       _ ->
                         do put $ Left $ Json.Array V.empty
                            _ <- ma
                            Left res <- get
                            lift $ put res
    , withField =
          \ma ->
              do fields <- get
                 case fields of
                   Right fields' ->
                       do ((index, Json.Object o), rest) <- takeEmptyField fields' []
                          let [(key, Json.Null)] = M.toList o
                          _ <- ma
                          value <- lift get
                          let innerObj = Json.Object $ M.fromList [(key, value)]
                          put $ Right $ (index, Json.object [(key, value)]):rest
                   Left (Json.Array ar) ->
                       do ma
                          value <- lift get
                          put $ Left $ Json.Array $ ar `V.snoc` value
                   f -> fail $ "No fields found at " ++ show f
    , writeRepetition =
          \ar ->
              case ar of
                []  ->
                    lift $ put $ array []
                [x] ->
                    do smartPut sFormat x
                       ar' <- lift get
                       lift $ put $ array [ar']
                xs  ->
                    do accArray [] ar (writeSmart sFormat)
                       ar <- lift get
                       lift $ put $ arConcat ar
    , writeMaybe =
          \ma ->
              case ma of
                Just a -> smartPut sFormat a
                Nothing ->
                    do lift $ put Json.Null
                       return ()
    }
    where mkArray (Json.Array ar) i = Json.Array $ ar `V.snoc` Json.Number i
          mkArray val i = array $ val:[Json.Number i]
          takeEmptyField [] notnull =
              fail "Encoding failure. Got more fields than expected for constructor."
          takeEmptyField map notnull =
                 case map of
                   h@(_, Json.Object o):t ->
                       case M.toList o of
                         [(_, Json.Null)] ->
                             return (h, notnull ++ tail map)
                         _ -> takeEmptyField t (h:notnull)
                   x:t -> takeEmptyField t (x:notnull)

-------------------------------------------------------------------------------
--  Versioned parsing
-------------------------------------------------------------------------------

type CurrentFields = State [(Int, Maybe String)]

pFormat :: ParseFormat (FailT (ReaderT Json.Value (StateT Json.Value CurrentFields)))
pFormat
    = ParseFormat
    { mkGetter =
          \_ prevVer ->
              return $
              do (version, rest) <- readVersion
                 case version of
                   Just v ->
                       case constructGetterFromVersion pFormat v kind of
                         Right getter ->
                             local (const rest) getter 
                         Left msg -> fail msg
                   Nothing -> readSmart pFormat
    , readCons =
          \cons ->
              do val <- ask
                 let conNames@(cn:_) = map (cname . fst) cons
                     parsers = map snd cons
                     conFields = map (cfields . fst) cons
                 case cons of
                   [] ->
                       noCons
                   [(x, parser)] ->
                       case val of
                         obj@(Json.Object _) ->
                             do _ <- putFieldsFromObj cn cons
                                lift $ put obj
                                local (const obj) parser
                         ar@(Json.Array _) ->
                             do _ <- putFieldsFromArr ar
                                lift $ put ar
                                local (const ar) parser
                         otherPrim ->
                             case cfields x of
                               Empty ->
                                   do lift $ put otherPrim
                                      local (const otherPrim) parser
                               NF 0 ->
                                   do lift $ put otherPrim
                                      local (const otherPrim) parser
                               NF 1 ->
                                   do lift $ put otherPrim
                                      local (const otherPrim) parser
                               _      ->
                                   mismatch "single-field constructor" (show otherPrim)
                   _ ->
                      case val of
                        Json.Object obj ->
                            if M.member (T.pack "tag") obj
                               then
                                   do let Just (Json.String con) = M.lookup (T.pack "tag") obj
                                      case M.lookup "contents" obj of
                                        Just args ->
                                            case lookup con (zip conNames parsers) of
                                              Just parser ->
                                                  do putFieldsFromObj con cons
                                                     lift $ put args
                                                     local (const args) parser
                                              Nothing ->
                                                  conLookupErr (T.unpack con) (show conNames)
                                        Nothing ->
                                            do let args = M.delete (T.pack "tag") obj
                                               case lookup con (zip conNames parsers) of
                                                 Just parser ->
                                                     do _ <- putFieldsFromObj con cons
                                                        lift $ put $ object args
                                                        local (const $ object args) parser
                                                 Nothing ->
                                                     conLookupErr (T.unpack con) (show conNames)
                               else mismatch "tagged type" (show obj)
                        ar@(Json.Array _) ->
                            case fromArray ar of
                              o@(Json.Object _):xs ->
                                    do lift $ put o
                                       res <- local (const o) (readCons pFormat cons)
                                       lift $ put $ array xs
                                       return res
                              nameOrField@(Json.String tag):xs ->
                                  case lookup tag (zip conNames parsers) of
                                    Just parser ->
                                        do lift $ put nameOrField
                                           res <- local (const nameOrField) parser
                                           lift $ put $ array xs
                                           return res
                                    Nothing ->
                                        conLookupErr (show tag) (show conNames)
                              f -> mismatch "tagged type" (show f)
                        tag@(Json.String s) ->
                            case lookup s (zip conNames parsers) of
                              Just parser ->
                                  local (const tag) parser
                              Nothing ->
                                  conLookupErr (show tag) (show conNames)
                        _ ->
                            mismatch "tagged type" (show val)
    , readField =
          \ma ->
              do fields <- lift $ lift $ lift get
                 case fields of
                   [] -> ma
                   (h, Nothing):t ->
                       do v <- ask
                          case v of
                            ar@(Json.Array a) ->
                                 do res <-
                                        local (const $ array $ drop h $ fromArray ar) ma
                                    lift $ lift $ lift $ put t
                                    return res
                            n ->
                                 do res <- local (const n) ma
                                    lift $ lift $ lift $ put t
                                    return res
                   (h, Just lab):t ->
                       do Json.Object obj <- ask
                          let index = T.pack $ show h
                          let label = T.pack lab
                              labField = M.lookup index obj
                          case labField of
                            Just (Json.Object obj') ->
                                case M.toList obj' of
                                  [(label, val)] ->
                                      do res <- local (const val) ma
                                         lift $ lift $ lift $ put t
                                         return res
                                  f ->
                                      mismatch ("labeled field " ++ T.unpack label) (show obj')
                            Nothing ->
                                mismatch ("field with index " ++ T.unpack index) (show obj)
    , readRepetition =
          do val <- ask
             case val of
               Json.Array ar ->
                   case V.toList ar of
                     ar1@(Json.Array _):_ ->
                         do lift $ put ar1
                            local (const ar1) $
                                do getSmartGet pFormat >>= replicateM (length $ fromArray ar1)
                     _ ->
                         forM (V.toList ar) $ \el -> local (const el) (readSmart pFormat)
               _ -> mismatch "Array" (show val)
    , readInt =
          do x <- ask
             case x of
               Json.Number n ->
                    return $ floor n
               ar@(Json.Array _) ->
                      case fromArray ar of
                        Json.Number n:xs ->
                            return $ floor n
                        _ ->
                            mismatch "Number" (show x)
               _ -> mismatch "Number" (show x)
    , readChar =
          do x <- ask
             case x of
               Json.String s ->
                   let str = T.unpack s in
                   case str of
                     [x] ->
                         return x
                     _ ->
                         mismatch "Char" str
               ar@(Json.Array _) ->
                   case fromArray ar of
                     Json.String s:xs ->
                         let str = T.unpack s in
                         case str of
                           [x] ->
                               return x
                           _ ->
                               mismatch "Char" (T.unpack s)
                     _ -> mismatch "Char" (show x)
               _ -> mismatch "Char" (show x)
    , readBool =
          do x <- ask
             case x of
               Json.Bool b -> return b
               ar@(Json.Array _) ->
                      case fromArray ar of
                        Json.Bool b:xs -> return b
                        _ -> mismatch "Bool" (show x)
               _ -> mismatch "Bool" (show x)
    , readDouble =
          do x <- ask
             case x of
               Json.Number d -> return $ realToFrac d
               ar@(Json.Array _) ->
                      case fromArray ar of
                        Json.Number d:xs -> return $ realToFrac d
                        _ -> mismatch "Number" (show x)
               _ -> mismatch "Number" (show x)
    , readString =
          do x <- ask
             case x of
               Json.String s -> return $ T.unpack s
               ar@(Json.Array _) ->
                   case fromArray ar of
                     Json.String s:xs -> return $ T.unpack s
                     _ -> mismatch "String" (show x)
               _ -> mismatch "String" (show x)
    , readMaybe =
          do x <- ask
             case x of
               Json.Null -> return Nothing
               ar@(Json.Array _) ->
                  case fromArray ar of
                    Json.Null:xs -> return Nothing
                    xs ->
                        do lift $ put ar
                           liftM Just $ smartGet pFormat
               val ->
                   do lift $ put val
                      liftM Just $ smartGet pFormat
    , readBS =
          do x <- ask
             case x of
               Json.String s -> return $ TE.encodeUtf8 s
               ar@(Json.Array _) ->
                case fromArray ar of
                  Json.String s:xs -> return $ TE.encodeUtf8 s
                  _ -> mismatch "ByteString" (show x)
               _ -> mismatch "ByteString" (show x)
    , readText =
          do x <- ask
             case x of
               Json.String s -> return s
               ar@(Json.Array _) ->
                case fromArray ar of
                  Json.String s:xs -> return s
                  _ -> mismatch "Text" (show x)
               _ -> mismatch "Text" (show x)
    }
    where withoutVersion o@(Json.Object obj)
              | M.member (T.pack "version") obj && M.member (T.pack "object") obj
              = fromJust $ M.lookup (T.pack "object") obj
              | M.member (T.pack "version") obj
              = Json.Object $ M.delete (T.pack "version") obj
              | otherwise = o
          withoutVersion v = v

          readVersion =
              do val <- lift get
                 case val of
                   o@(Json.Object obj) ->
                     case M.lookup (T.pack "version") obj of
                       Just (Json.Number ver) ->
                           return (Just $ Version $ floor ver, withoutVersion o)
                       _ ->
                           case M.toList obj of
                             [] -> return (Nothing, o)
                             (index, Json.Object obj'):xs ->
                                 case M.toList obj' of
                                   [(label, cont)] ->
                                       do lift $ put cont
                                          v <- readVersion
                                          lift $ put $ Json.object xs
                                          return v
                             (k, v):xs ->
                                 do lift $ put v
                                    v <- readVersion
                                    lift $ put $ Json.object xs
                                    return v
                   a@(Json.Array ar) ->
                      case V.length ar of
                        0 -> return (Nothing, Json.Null)
                        n ->
                            case V.toList ar of
                              [val] ->
                                  do lift $ put val
                                     readVersion
                              val:xs ->
                                  do lift $ put val
                                     (v, rest) <- readVersion
                                     lift $ put $ array xs
                                     return (v, array $ rest:xs)
                   v -> return (Nothing, v)

          putFieldsFromObj con cons = 
              do let conFields = map (cfields . fst) cons
                     conNames = map (cname . fst) cons
                     Just cf = lookup con (zip conNames conFields)
                     fields
                         = case cf of
                             Empty -> []
                             NF i -> zip [0..i-1] (repeat Nothing)
                             LF lbs ->
                                 zip [0..] (map (Just . T.unpack) lbs)
                 lift $ lift $ lift $ put fields

          putFieldsFromArr ar =
              do let l = length $ fromArray ar
                     fields = zip [0..l-1] (repeat Nothing)
                 lift $ lift $ lift $ put fields

-------------------------------------------------------------------------------
-- Unversioned serialization
-------------------------------------------------------------------------------

sFormatUnvers :: SerializationFormat (StateT (Either Json.Value [JT.Pair]) (State Json.Value))
sFormatUnvers
    = SerializationFormat
    { mkPutter = \_ v -> return $ writeSmart sFormatUnvers
    , withCons =
          \cons ma ->
              if ctagged cons
                 then case cfields cons of
                        Empty ->
                          lift $ put $ Json.String $ cname cons
                        NF 0 ->
                          lift $ put $ Json.object [("tag", Json.String $ cname cons),
                                                    ("contents", Json.Array V.empty)]
                        NF i ->
                          do put $ Left $ Json.Array V.empty
                             _ <- ma
                             Left res <- get
                             let resObj = Json.object [("tag", Json.String $ cname cons),
                                                       ("contents", arConcat res)]
                             lift $ put resObj
                        LF ls ->
                          do put $ Right $ zip ls (repeat Json.Null)
                             _ <- ma
                             Right res <- get
                             let resObj 
                                  = Json.object $ ("tag", Json.String $ cname cons):res
                             lift $ put resObj
                 else case cfields cons of
                       LF ls ->
                         do let fields = zip ls (repeat Json.Null)
                            put $ Right fields
                            _ <- ma
                            Right res <- get
                            lift $ put $ Json.object res
                       _ ->
                         do put $ Left $ Json.Array V.empty
                            _ <- ma
                            Left res <- get
                            lift $ put $ arConcat res
    
    , withField =
          \ma ->
              do fields <- get
                 case fields of
                   Right fields' ->
                         do ((key, Json.Null), rest) <- takeEmptyField fields' []
                            _ <- ma
                            value <- lift get
                            put $ Right $ (key, value):rest
                   Left (Json.Array ar) ->
                       do ma
                          value <- lift get
                          put $ Left $ Json.Array $ ar `V.snoc` value
                   f -> fail $ "No fields found at " ++ show f
    
    , writeRepetition =
          \ar ->
              case ar of
                [] -> lift $ put $ array []
                [x] ->
                    do writeSmart sFormatUnvers x
                       el <- lift get
                       lift $ put $ array [el]
                _ ->
                    do accArray [] ar (writeSmart sFormatUnvers)
                       ar <- lift get
                       lift $ put $ arConcat ar
    , writeInt =
          \i ->
              do lift $ put $ Json.Number $ fromIntegral i
                 return ()
    , writeInteger =
          \i ->
              do lift $ put $ Json.Number $ fromIntegral i
                 return ()
    , writeChar =
          \c ->
              do lift $ put $ Json.String $ T.pack [c]
                 return ()
    , writeBool =
          \b ->
              do lift $ put $ Json.Bool b
                 return ()
    , writeString =
          \s ->
              do lift $ put $ Json.String $ T.pack s
                 return ()
    , writeDouble =
          \d ->
              do lift $ put $ Json.Number $ fromFloatDigits d
                 return ()
    , writeMaybe =
          \ma ->
              case ma of
                Just a -> writeSmart sFormatUnvers a
                Nothing ->
                    do lift $ put Json.Null
                       return ()
    , writeBS =
          \bs ->
              do lift $ put $ Json.String $ TE.decodeUtf8 bs
                 return ()
    , writeText =
          \text ->
              do lift $ put $ Json.String text
                 return ()
    }
    where takeEmptyField [] notnull =
              fail "Encoding failure. Got more fields than expected for constructor."
          takeEmptyField map notnull =
              case map of
                h@(_, Json.Null):t -> return (h, notnull ++ t)
                h:t -> takeEmptyField t (h:notnull)

-------------------------------------------------------------------------------
--  Unversioned parsing
-------------------------------------------------------------------------------

pFormatUnvers :: ParseFormat (FailT (ReaderT Json.Value (State [String])))
pFormatUnvers
    = ParseFormat
    { mkGetter = \_ _ -> return $ readSmart pFormatUnvers 
    , readCons =
        \cons ->
            do val <- ask
               let conNames@(cn:_) = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case cons of
                 [] -> noCons
                 [(h, parser)] -> case val of
                        obj@(Json.Object _) ->
                            do _ <- putFieldsFromObj cn cons
                               local (const obj) parser
                        ar@(Json.Array _) ->
                            do _ <- putFieldsFromArr ar
                               local (const ar) parser
                        otherPrim ->
                            case cfields h of
                              Empty ->
                                  local (const otherPrim) parser
                              NF 0 ->
                                  local (const otherPrim) parser
                              NF 1 ->
                                  local (const otherPrim) parser
                              _      ->
                                  mismatch "single-field constructor" (show otherPrim)
                 _ ->
                    case val of
                      Json.Object obj ->
                          if M.member (T.pack "tag") obj
                             then
                                 do let Just (Json.String con) = M.lookup (T.pack "tag") obj
                                    case M.lookup "contents" obj of
                                      Just args ->
                                          case lookup con (zip conNames parsers) of
                                            Just parser ->
                                                do putFieldsFromObj con cons
                                                   local (const args) parser
                                            Nothing ->
                                                conLookupErr (T.unpack con) (show conNames)
                                      Nothing ->
                                          do let args = M.delete (T.pack "tag") obj
                                             case lookup con (zip conNames parsers) of
                                               Just parser ->
                                                   do _ <- putFieldsFromObj con cons
                                                      local (const $ object args) parser
                                               Nothing ->
                                                conLookupErr (T.unpack con) (show conNames)
                             else mismatch "tagged type" (show obj)
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const o) (readCons pFormatUnvers cons)
                            nameOrField@(Json.String tag):_ ->
                                case lookup tag (zip conNames parsers) of
                                  Just parser ->
                                      local (const nameOrField) parser
                                  Nothing ->
                                      conLookupErr (show tag) (show conNames)
                            f -> mismatch "tagged type" (show f)
                      tag@(Json.String s) ->
                          case lookup s (zip conNames parsers) of
                            Just parser ->
                                local (const tag) parser
                            Nothing ->
                                conLookupErr (show tag) (show conNames)
    , readField =                      
          \ma ->
              do fields <- lift $ lift get
                 case fields of
                   [] -> ma
                   h:t ->
                       case reads h of
                         [(num, "")] ->
                             do v <- ask
                                case v of
                                  ar@(Json.Array a)  ->
                                      do res <-
                                           local (const $ array $ drop num $ fromArray ar) ma
                                         put t
                                         return res
                                  n ->
                                      do res <- local (const n) ma
                                         put t
                                         return res
                                          
                         [] ->
                             do o@(Json.Object _) <- ask
                                let field = T.pack h
                                case lookup field $ fromObject o of
                                  Just test ->
                                      do res <- local (const test) ma
                                         put t
                                         return res
                                  Nothing ->
                                      fail $ show o ++ "Fields: " ++ show fields
    , readRepetition =
          do val <- ask
             case val of
               Json.Array ar ->
                   case V.toList ar of
                     ar1@(Json.Array _):_ ->
                         local (const ar1) (readRepetition pFormatUnvers)
                     _ ->
                         forM (V.toList ar) (\el -> local (const el) $ readSmart pFormatUnvers)
               _ -> mismatch "Array" (show val)
    , readInt =
          do x <- ask
             case x of
               Json.Number n ->
                    return $ floor n
               ar@(Json.Array _) ->
                      case fromArray ar of
                        Json.Number n:xs -> return $ floor n
                        _ -> mismatch "Number" (show x)
               _ -> mismatch "Number" (show x)
    , readChar =
          do x <- ask
             case x of
               Json.String s ->
                   let str = T.unpack s in
                   case str of
                     [c] -> return c
                     _ -> mismatch "Char" str
               ar@(Json.Array _) ->
                   case fromArray ar of
                     Json.String s:xs ->
                         let str = T.unpack s in
                         case str of
                           [c] -> return c
                           _ -> mismatch "Char" (T.unpack s)
                     _ -> mismatch "Char" (show x)
               _ -> mismatch "Char" (show x)
    , readBool =
          do x <- ask
             case x of
               (Json.Bool b) -> return b
               ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Bool b:xs -> return b
                  _ -> mismatch "Bool" (show x)
               _ -> mismatch "Bool" (show x)
    , readDouble =
          do x <- ask
             case x of
               (Json.Number d) -> return $ realToFrac d
               ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Number d:xs -> return $ realToFrac d
                  _ -> mismatch "Number" (show x)
               _ -> mismatch "Number" (show x)
    , readString =
          do x <- ask
             case x of
               (Json.String s) -> return $ T.unpack s
               ar@(Json.Array _) ->
                case fromArray ar of
                  Json.String s:xs -> return$ T.unpack s
                  _ -> mismatch "String" (show x)
               _ -> mismatch "String" (show x)
    , readMaybe =
          do x <- ask
             case x of
               Json.Null -> return Nothing
               ar@(Json.Array _) ->
                  case fromArray ar of
                    Json.Null:xs -> return Nothing
                    xs -> liftM Just $ readSmart pFormatUnvers
               xs -> liftM Just $ readSmart pFormatUnvers
    , readBS =
          do x <- ask
             case x of
               Json.String s -> return $ TE.encodeUtf8 s
               ar@(Json.Array _) ->
                   case fromArray ar of
                     Json.String s:xs -> return $ TE.encodeUtf8 s
                     _ -> mismatch "ByteString" (show x)
               _ -> mismatch "ByteString" (show x)
    , readText =
          do x <- ask
             case x of
               Json.String s -> return s
               ar@(Json.Array _) ->
                   case fromArray ar of
                     Json.String s:xs -> return s
                     _ -> mismatch "Text" (show x)
               _ -> mismatch "Text" (show x)
    }
    where putFieldsFromObj con cons = 
              do let conFields = map (cfields . fst) cons
                     conNames = map (cname . fst) cons
                     Just cf = lookup con (zip conNames conFields)
                     fields
                         = case cf of
                             Empty -> []
                             NF i -> map show [0..i-1]
                             LF lbs -> map T.unpack lbs
                 put fields
          putFieldsFromArr ar =
              do let l = length $ fromArray ar
                     fields = [0..l-1]
                 put $ map show fields


-------------------------------------------------------------------------------
--  Helper functions
-------------------------------------------------------------------------------

fromObject (Json.Object o) = M.toList o
fromObject val = [("object", val)]

fromArray (Json.Array a) = V.toList a
fromArray val = [val]

array = Json.Array . V.fromList
object = Json.Object

accArray xs [] wf = return ()
accArray xs (el:ar) wf =
       do wf el
          val' <- lift get
          let val = case val' of
                      Json.Array ar' -> V.toList ar'
                      p -> [p]
          let acc = xs ++ val
          lift $ put $ array (xs ++ val)
          accArray acc ar wf
          return ()

arConcat :: Json.Value -> Json.Value
arConcat a@(Json.Array ar)
    = let vs = V.toList ar in
      case vs of
        [x] ->
          case vs of
            [Json.Null] -> array []
            _           -> x
        _ -> a
arConcat o = o
