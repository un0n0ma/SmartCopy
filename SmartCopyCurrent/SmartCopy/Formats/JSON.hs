{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module SmartCopy.Formats.JSON
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       , serializeWith
       , encodeUnvers
       , encodeSmart
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

import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Utils (fromFloatDigits)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Aeson as Json (Value(..), object)
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
parseSmart = runParser (fromEitherM $ smartGet pFormat)
    where runParser action value =
              evalState (evalStateT (runReaderT (runFailT action) (Right value)) value) []

serializeUnvers a = runSerialization (writeSmart sFormatUnvers a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseUnvers :: SmartCopy a => Json.Value -> Fail a
parseUnvers = runParser (fromEitherM $ readSmart pFormatUnvers)
    where runParser action value =
              evalState (runReaderT (runFailT action) (Right value)) []

serializeWith a version = runSerialization (smartPutWithVersion sFormat a version)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

-------------------------------------------------------------------------------
--  Versioned serialization
-------------------------------------------------------------------------------

sFormat
    = sFormatUnvers
    { mkPutter =
          \ver ->
          return $ \a ->
              do writeSmart sFormat a
                 res <- lift get
                 let versObj = [("version", Json.Number $ fromIntegral $ unVersion ver)]
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
              case length ar of
                0 -> lift $ put $ array []
                1 -> do smartPut sFormat (head ar)
                        ar <- lift get
                        lift $ put $ array [ar]
                n -> do accArray [] ar (writeSmart sFormat)
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
                 case head map of
                   (_, Json.Object o) ->
                       case M.toList o of
                         [(_, Json.Null)] ->
                             return (head map, notnull ++ tail map)
                         _ -> takeEmptyField (tail map) (head map:notnull)
                   x -> takeEmptyField (tail map) (x:notnull)

-------------------------------------------------------------------------------
--  Versioned parsing
-------------------------------------------------------------------------------

type CurrentFields = State [(Int, Maybe String)]

pFormat :: ParseFormat (FailT (ReaderT (Either String Json.Value) (StateT Json.Value CurrentFields)))
pFormat
    = ParseFormat
    { mkGetter =
          return $
          do (version, rest) <- readVersion
             case version of
               Just v ->
                   case constructGetterFromVersion pFormat v kind of
                     Right getter ->
                         local (const $ Right rest) getter 
                     Left msg -> return $ Left msg
               Nothing -> readSmart pFormat
    , withLookahead =
          \_ ma mb ->
          do val <- ask
             res <- ma
             case res of
               Left _ ->
                   local (const val) mb
               r@(Right _) ->
                   return r
    , readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case length cons of
                 0 -> noCons
                 1 -> case val of
                        Right obj@(Json.Object _) ->
                            do let con = head conNames
                                   parser = head parsers
                               _ <- putFieldsFromObj con cons
                               lift $ put obj
                               local (const $ Right obj) parser
                        Right ar@(Json.Array _) ->
                            do _ <- putFieldsFromArr ar
                               lift $ put ar
                               local (const $ Right ar) (head parsers)
                        Right otherPrim ->
                            do let parser = snd $ head cons
                               case cfields $ fst $ head cons of
                                 Empty ->
                                     do lift $ put otherPrim
                                        local (const $ Right otherPrim) parser
                                 NF 0 ->
                                     do lift $ put otherPrim
                                        local (const $ Right otherPrim) parser
                                 NF 1 ->
                                     do lift $ put otherPrim
                                        local (const $ Right otherPrim) parser
                                 _      -> mismatch "single-field constructor" (show otherPrim)
                        Left msg -> return $ Left msg
                 _ ->
                    case val of
                      Right (Json.Object obj) ->
                          case M.member (T.pack "tag") obj of
                            True -> do
                                let Just (Json.String con) = M.lookup (T.pack "tag") obj
                                case M.lookup "contents" obj of
                                  Just args ->
                                      case lookup con (zip conNames parsers) of
                                        Just parser ->
                                            do putFieldsFromObj con cons
                                               lift $ put args
                                               local (const $ Right args) parser
                                        Nothing ->
                                            conLookupErr (T.unpack con) (show conNames)
                                  Nothing ->
                                      do let args = M.delete (T.pack "tag") obj
                                         case lookup con (zip conNames parsers) of
                                           Just parser ->
                                               do _ <- putFieldsFromObj con cons
                                                  lift $ put $ object args
                                                  local (const $ Right $ object args) parser
                                           Nothing ->
                                               conLookupErr (T.unpack con) (show conNames)
                            f -> mismatch "tagged type" (show obj)
                      Right ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):xs ->
                                  do lift $ put o
                                     res <- local (const $ Right o) (readCons pFormat cons)
                                     lift $ put $ array xs
                                     return res
                            nameOrField@(Json.String tag):xs ->
                                case lookup tag (zip conNames parsers) of
                                  Just parser ->
                                      do lift $ put nameOrField
                                         res <- local (const $ Right nameOrField) parser
                                         lift $ put $ array xs
                                         return res
                                  Nothing ->
                                      conLookupErr (show tag) (show conNames)
                            f -> mismatch "tagged type" (show f)
                      Right tag@(Json.String s) ->
                          case lookup s (zip conNames parsers) of
                            Just parser ->
                                local (const $ Right tag) parser
                            Nothing ->
                                conLookupErr (show tag) (show conNames)
                      _ ->
                          mismatch "tagged type" (show val)
  --  , readSum =
    , readField =
        \ma ->
            do fields <- lift $ lift $ lift get
               case fields of
                 [] -> ma
                 xs ->
                     case snd $ head xs of
                       Nothing ->
                           do v <- ask
                              case v of
                                Right ar@(Json.Array a) ->
                                     do res <-
                                          local (const $ Right $ array $ drop
                                                (fst $ head xs) $ fromArray ar) ma
                                        lift $ lift $ lift $ put $ tail xs
                                        return res
                                Right n ->
                                     do res <- local (const $ Right n) ma
                                        lift $ lift $ lift $ put $ tail xs
                                        return res
                                Left msg -> return $ Left msg   
                                        
                       Just lab ->
                           do Right (Json.Object obj) <- ask
                              let index = T.pack $ show $ fst $ head xs
                              let label = T.pack lab
                                  labField = M.lookup index obj
                              case labField of
                                Just (Json.Object obj') ->
                                    case M.toList obj' of
                                      [(label, val)] ->
                                          do res <- local (const $ Right val) ma
                                             lift $ lift $ lift $ put $ tail xs
                                             return res
                                      f ->
                                          mismatch ("labeled field " ++ T.unpack label) (show obj')
                                         
                                Nothing ->
                                    mismatch ("field with index " ++ T.unpack index) (show obj)

    , readRepetition =
            do val <- ask
               case val of
                 Right (Json.Array ar) ->
                     case V.toList ar of
                       ar1@(Json.Array _):_ ->
                           do lift $ put ar1
                              local (const $ Right ar1) $
                                  do res <- getSmartGet pFormat >>=
                                            replicateM (length $ fromArray ar1)
                                     if null (lefts res)
                                        then return $ Right $ rights res
                                        else return $ Left []
                       _ ->
                           do res <- forM (V.toList ar) (\el -> local (const $ Right el)
                                          (readSmart pFormat))
                              if null (lefts res)
                                 then return $ Right $ rights res
                                 else return $ Left []
                                
                 _ -> mismatch "Array" (show val)
    , readInt =
        do x <- ask
           case x of
             Right (Json.Number n) ->
                  return $ Right $ floor n
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number n:xs -> return $ Right $ floor n
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    
    , readChar =
        do x <- ask
           case x of
             Right (Json.String s) ->
                  let str = T.unpack s in
                  if length str == 1
                     then return $ Right $ head str
                     else mismatch "Char" (T.unpack s)
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs ->
                          let str = T.unpack s in
                          if length str == 1
                             then return $ Right $ head str
                             else mismatch "Char" (T.unpack s)
                      _ -> mismatch "Char" (show x)
             _ -> mismatch "Char" (show x)
    , readBool =
        do x <- ask
           case x of
             Right (Json.Bool b) -> return $ Right b
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Bool b:xs -> return $ Right b
                      _ -> mismatch "Bool" (show x)
             _ -> mismatch "Bool" (show x)
    , readDouble =
        do x <- ask
           case x of
             Right (Json.Number d) -> return $ Right $ realToFrac d
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number d:xs -> return $ Right $ realToFrac d
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    , readString =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right $ T.unpack s
             Right ar@(Json.Array _) ->
                 case fromArray ar of
                   Json.String s:xs -> return $ Right $ T.unpack s
                   _ -> mismatch "String" (show x)
             _ -> mismatch "String" (show x)
    , readMaybe =
        do x <- ask
           case x of
             Right Json.Null -> return $ Right Nothing
             Right ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Null:xs -> return $ Right Nothing
                  xs ->
                      do lift $ put ar
                         res <- smartGet pFormat
                         case res of
                           Right a ->
                               return $ Right $ Just a
                           Left msg ->
                               return $ Left msg
             Right val ->
                 do lift $ put val
                    res <- smartGet pFormat
                    case res of
                      Right a ->
                          return $ Right $ Just a
                      Left msg ->
                          return $ Left msg
             Left msg -> return $ Left msg                              
    , readBS =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right $ TE.encodeUtf8 s
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ Right $ TE.encodeUtf8 s
                      _ -> mismatch "ByteString" (show x)
             _ -> mismatch "ByteString" (show x)
    , readText =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right s
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ Right s
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
                           do case M.toList obj of
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
    { mkPutter = \_ -> return $ writeSmart sFormatUnvers 
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
              case length ar of
                0 -> lift $ put $ array []
                1 ->
                    do writeSmart sFormatUnvers (head ar)
                       el <- lift get
                       lift $ put $ array [el]
                n ->
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
                 case head map of
                   f@(_, Json.Null) -> return (f, notnull ++ tail map)
                   x -> takeEmptyField (tail map) (x:notnull)

-------------------------------------------------------------------------------
--  Unversioned parsing
-------------------------------------------------------------------------------

pFormatUnvers :: ParseFormat (FailT (ReaderT (Either String Json.Value) (State [String])))
pFormatUnvers
    = ParseFormat
    { mkGetter = return $ readSmart pFormatUnvers 
    , withLookahead =
          \_ ma mb ->
          do val <- ask
             res <- ma
             case res of
               Left _ ->
                   local (const val) mb
               r@(Right _) ->
                   return r
    , readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case length cons of
                 0 -> noCons
                 1 -> case val of
                        Right (obj@(Json.Object _)) ->
                            do let con = head conNames
                                   parser = head parsers
                               _ <- putFieldsFromObj con cons
                               local (const $ Right obj) parser
                        Right (ar@(Json.Array _)) ->
                            do _ <- putFieldsFromArr ar
                               local (const $ Right ar) (head parsers)
                        Right otherPrim ->
                            case cfields $ fst $ head cons of
                              Empty ->
                                  do let parser = snd $ head cons
                                     local (const $ Right otherPrim) parser
                              NF 0 ->
                                  do let parser = snd $ head cons
                                     local (const $ Right otherPrim) parser
                              NF 1 ->
                                  do let parser = snd $ head cons
                                     local (const $ Right otherPrim) parser
                              _      -> mismatch "single-field constructor" (show otherPrim)
                        Left msg -> return $ Left msg
                 _ ->
                    case val of
                      Right (Json.Object obj) ->
                          case M.member (T.pack "tag") obj of
                            True -> do
                                let Just (Json.String con) = M.lookup (T.pack "tag") obj
                                case M.lookup "contents" obj of
                                  Just args ->
                                      case lookup con (zip conNames parsers) of
                                        Just parser ->
                                            do putFieldsFromObj con cons
                                               local (const $ Right args) parser
                                        Nothing ->
                                            conLookupErr (T.unpack con) (show conNames)
                                  Nothing ->
                                      do let args = M.delete (T.pack "tag") obj
                                         case lookup con (zip conNames parsers) of
                                           Just parser ->
                                               do _ <- putFieldsFromObj con cons
                                                  local (const $ Right $ object args) parser
                                           Nothing ->
                                            conLookupErr (T.unpack con) (show conNames)
                            f -> mismatch "tagged type" (show obj)
                      Right ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const $ Right o) (readCons pFormatUnvers cons)
                            nameOrField@(Json.String tag):_ ->
                                case lookup tag (zip conNames parsers) of
                                  Just parser ->
                                      local (const $ Right nameOrField) parser
                                  Nothing ->
                                      conLookupErr (show tag) (show conNames)
                            f -> mismatch "tagged type" (show f)
                      Right tag@(Json.String s) ->
                          case lookup s (zip conNames parsers) of
                            Just parser ->
                                local (const $ Right tag) parser
                            Nothing ->
                                conLookupErr (show tag) (show conNames)
                      Left msg ->
                          mismatch "tagged type" msg
    , readField =                      
        \ma ->
            do fields <- lift $ lift get
               case fields of
                 [] -> ma
                 xs ->
                     case reads $ head xs of
                       [(num, "")] ->
                           do v <- ask
                              case v of
                                Right ar@(Json.Array a)  ->
                                    do res <-
                                         local (const $ Right $ array $ drop num $ fromArray ar) ma
                                       put $ tail xs
                                       return res
                                Right n ->
                                    do res <- local (const $ Right n) ma
                                       put $ tail xs
                                       return res
                                Left msg -> return $ Left msg
                                        
                       [] ->
                           do Right o@(Json.Object _) <- ask
                              let field = T.pack $ head xs
                              case lookup field $ fromObject o of
                                Just test ->
                                    do res <- local (const $ Right test) ma
                                       put $ tail xs
                                       return res
                                Nothing ->
                                    fail $ show o ++ "Fields: " ++ show xs
                        
    , readRepetition =
            do val <- ask
               case val of
                 Right (Json.Array ar) ->
                     case V.toList ar of
                       ar1@(Json.Array _):_ ->
                           local (const $ Right ar1) (readRepetition pFormatUnvers)
                       _ ->
                           do res <- forM (V.toList ar) (\el -> local (const $ Right el) $
                                          readSmart pFormatUnvers)
                              if null (lefts res)
                                 then return $ Right $ rights res
                                 else return $ Left []
                 _ -> mismatch "Array" (show val)
    , readInt =
        do x <- ask
           case x of
             Right (Json.Number n) ->
                  return $ Right $ floor n
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number n:xs -> return $ Right $ floor n
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    
    , readChar =
        do x <- ask
           case x of
             Right (Json.String s) ->
                  let str = T.unpack s in
                  if length str == 1
                     then return $ Right $ head str
                     else mismatch "Char" (T.unpack s)
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs ->
                          let str = T.unpack s in
                          if length str == 1
                             then return $ Right $ head str
                             else mismatch "Char" (T.unpack s)
                      _ -> mismatch "Char" (show x)
             _ -> mismatch "Char" (show x)
    , readBool =
        do x <- ask
           case x of
             Right (Json.Bool b) -> return $ Right b
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Bool b:xs -> return $ Right b
                      _ -> mismatch "Bool" (show x)
             _ -> mismatch "Bool" (show x)
    , readDouble =
        do x <- ask
           case x of
             Right (Json.Number d) -> return $ Right $ realToFrac d
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number d:xs -> return $ Right $ realToFrac d
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    , readString =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right $ T.unpack s
             Right ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ Right $ T.unpack s
                      _ -> mismatch "String" (show x)
             _ -> mismatch "String" (show x)
    , readMaybe =
        do x <- ask
           case x of
             Right Json.Null -> return $ Right Nothing
             Right ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Null:xs -> return $ Right Nothing
                  xs ->
                      do res <- readSmart pFormatUnvers
                         either (return . Left) (return . Right . Just) res
             xs ->
                 do res <- readSmart pFormatUnvers
                    either (return . Left) (return . Right . Just) res
    , readBS =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right $ TE.encodeUtf8 s
             Right (ar@(Json.Array _)) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ Right $ TE.encodeUtf8 s
                      _ -> mismatch "ByteString" (show x)
             _ -> mismatch "ByteString" (show x)
    , readText =
        do x <- ask
           case x of
             Right (Json.String s) -> return $ Right s
             Right (ar@(Json.Array _)) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ Right s
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

accArray xs [] wf = return $ Right ()
accArray xs ar wf =
       do let el = head ar
          wf el
          val' <- lift get
          let val = case val' of
                      Json.Array ar -> V.toList ar
                      p -> [p]
          let acc = xs ++ val
          lift $ put $ array (xs ++ val)
          accArray acc (tail ar) wf
          return $ Right ()

arConcat :: Json.Value -> Json.Value
arConcat a@(Json.Array ar)
    = let vs = V.toList ar in
      case length vs of
        1 ->
          case vs of
            [Json.Null] -> array []
            _           -> head vs
        _ -> a
arConcat o = o
