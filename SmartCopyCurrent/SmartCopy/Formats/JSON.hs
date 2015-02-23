{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SmartCopy.Formats.JSON
       ( serializeSmart
       , parseSmart
       , serializeUnvers
       , parseUnvers
       , encode
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
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Aeson as Json (Value(..), object)
import qualified Data.Aeson.Types as JT (Pair)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
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


encode :: Json.Value -> LBS.ByteString
encode = encodeUtf8 . toLazyText . encodeToTextBuilder

-------------------------------------------------------------------------------
--  Run functions, versioned and unversioned
-------------------------------------------------------------------------------

serializeSmart a = runSerialization (smartPut sFormat a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseSmart :: SmartCopy a => Json.Value -> Fail a
parseSmart = runParser (smartGet pFormat)
    where runParser action value =
              evalState (evalStateT (runReaderT (runFailT action) value) (value, [])) []

serializeUnvers a = runSerialization (writeSmart sFormatUnvers a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseUnvers :: SmartCopy a => Json.Value -> Fail a
parseUnvers = runParser (readSmart pFormatUnvers)
    where runParser action value = evalState (runReaderT (runFailT action) value) []

-------------------------------------------------------------------------------
--  Versioned serialization
-------------------------------------------------------------------------------

sFormat
    = sFormatUnvers
    { withVersion =
          \ver ma ->
          do ma
             res <- lift get
             let versObj = [("version", Json.Number $ fromIntegral $ unVersion ver)]
                 resObj = case lookup (T.pack "version") (fromObject res) of
                            Just _ -> versObj ++ [("object", res)]
                            Nothing -> versObj ++ fromObject res
             lift $ put $ Json.object resObj
    , writeVersion = \_ -> return ()
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
                0 -> return ()
                1 -> do putter <- getSmartPut sFormat
                        putter (head ar)
                        ar <- lift get
                        lift $ put $ array [ar]
                n -> do putter <- getSmartPut sFormat
                        putter (head ar)
                        accArray [] ar (writeSmart sFormat)
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

pFormat :: ParseFormat (FailT (ReaderT Json.Value (StateT (Json.Value, [Int]) CurrentFields)))
pFormat
    = ParseFormat
    { readVersion =
        do (val, prevVersions) <- lift get
           case val of
             o@(Json.Object obj) ->
               case M.lookup (T.pack "version") obj of
                 Just (Json.Number ver) ->
                     do lift $ put (withoutVersion o, incremVers prevVersions)
                        return $ Just $ Version $ floor ver
                 _ ->
                     do let lastV = case prevVersions of
                                      [] -> T.pack "0"
                                      xs -> T.pack $ show $ last xs
                        case M.lookup lastV obj of
                          Just (Json.Object labField) ->
                              case M.toList labField of
                                [(_label, field)] ->
                                    do lift $ put (field, [])
                                       v <- readVersion pFormat
                                       let rest = Json.Object $ M.delete lastV obj
                                       lift $ put (rest, incremVers prevVersions)
                                       return v
                          Nothing ->
                              mismatch ("field with index "++T.unpack lastV) (show o)
             a@(Json.Array ar) ->
                case V.length ar of
                  0 -> fail $ show a --return $ Just $ Version 0. Fix this!!
                  n -> do let lastV = case prevVersions of
                                        [] -> 0
                                        xs -> last xs
                          lift $ put (V.head ar, [])
                          v <- readVersion pFormat
                          lift $ put (Json.Array $ V.tail ar, incremVers prevVersions)
                          return v
             v -> return $ Just $ Version 0
    , readVersioned =
        \ma ->
        do val <- ask
           case val of
             a@(Json.Array _) ->
                 case fromArray a of
                   [] -> ma
                   v:_ -> local (const $ withoutVersion v) ma
             o -> local (const $ withoutVersion o) ma
    , readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case length cons of
                 0 -> noCons
                 1 -> case val of
                        obj@(Json.Object _) ->
                            do let con = head conNames
                                   parser = head parsers
                               _ <- putFieldsFromObj con cons
                               local (const obj) parser
                        ar@(Json.Array _) ->
                            do _ <- putFieldsFromArr ar
                               lift $ put (ar, [])
                               local (const ar) (head parsers)
                        otherPrim ->
                            case cfields $ fst $ head cons of
                              NF 0 ->
                                  do let parser = snd $ head cons
                                     local (const otherPrim) parser
                              NF 1 ->
                                  do let parser = snd $ head cons
                                     local (const otherPrim) parser
                              _      -> mismatch "single-field constructor" (show otherPrim)
                 _ ->
                    case val of
                      Json.Object obj ->
                          case M.member (T.pack "tag") obj of
                            True -> do
                                let Just (Json.String con) = M.lookup (T.pack "tag") obj
                                case M.lookup "contents" obj of
                                  Just args ->
                                      case lookup con (zip conNames parsers) of
                                        Just parser ->
                                            do putFieldsFromObj con cons
                                               lift $ put (args, [])
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
                            f -> mismatch "tagged type" (show obj)
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):xs ->
                                  local (const o) (readCons pFormat cons)
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
                      _ ->
                          mismatch "tagged type" (show val)
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
                                Json.Array a  ->
                                     do res <- local (array . drop (fst $ head xs) . fromArray) ma
                                        lift $ lift $ lift $ put $ tail xs
                                        return res
                                n ->
                                     do res <- local (const n) ma
                                        lift $ lift $ lift $ put $ tail xs
                                        return res
                                        
                       Just lab ->
                           do Json.Object obj <- ask
                              let index = T.pack $ show $ fst $ head xs
                              let label = T.pack lab
                                  labField = M.lookup index obj
                              case labField of
                                Just (Json.Object obj') ->
                                    case M.toList obj' of
                                      [(label, val)] ->
                                          do res <- local (const val) ma
                                             lift $ lift $ lift $ put $ tail xs
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
                           do lift $ put (ar1, [])
                              local (const ar1) $
                                  getSmartGet pFormat >>=
                                  replicateM (length $ fromArray ar1)
                       _ -> do lift $ put (Json.Array ar, [])
                               forM (V.toList ar) (\el -> local (const el) (readSmart pFormat))
                       --- We don't "getSmartGet" here, because version is already
                       --- read in "readVersion".
                 _ -> mismatch "Array" (show val)
    , readInt =
        do x <- ask
           case x of
             Json.Number n ->
                  return $ PrimInt $ floor n
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number n:xs -> return $ PrimInt $ floor n
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    
    , readChar =
        do x <- ask
           case x of
             Json.String s ->
                  let str = T.unpack s in
                  if length str == 1
                     then return $ PrimChar $ head str
                     else mismatch "Char" (T.unpack s)
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs ->
                          let str = T.unpack s in
                          if length str == 1
                             then return $ PrimChar $ head str
                             else mismatch "Char" (T.unpack s)
                      _ -> mismatch "Char" (show x)
             _ -> mismatch "Char" (show x)
    , readBool =
        do x <- ask
           case x of
             Json.Bool b -> return $ PrimBool b
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Bool b:xs -> return $ PrimBool b
                      _ -> mismatch "Bool" (show x)
             _ -> mismatch "Bool" (show x)
    , readDouble =
        do x <- ask
           case x of
             Json.Number d -> return $ PrimDouble $ realToFrac d
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number d:xs -> return $ PrimDouble $ realToFrac d
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    , readString =
        do x <- ask
           case x of
             Json.String s -> return $ PrimString $ T.unpack s
             ar@(Json.Array _) ->
                 case fromArray ar of
                   Json.String s:xs -> return $ PrimString $ T.unpack s
                   _ -> mismatch "String" (show x)
             _ -> mismatch "String" (show x)
    , readMaybe =
        do x <- ask
           case x of
             Json.Null -> return Nothing
             ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Null:xs -> return Nothing
                  xs -> liftM Just $ smartGet pFormat
             xs -> liftM Just $ smartGet pFormat
    }
    where withoutVersion o@(Json.Object obj)
              | M.member (T.pack "version") obj && M.member (T.pack "object") obj
              = fromJust $ M.lookup (T.pack "object") obj
              | M.member (T.pack "version") obj
              = Json.Object $ M.delete (T.pack "version") obj
              | otherwise = o
          withoutVersion v = v
          incremVers prevVersions =
              case prevVersions of
                [] -> [0]
                xs -> xs ++ [succ $ last xs]
          putFieldsFromObj con cons = 
              do let conFields = map (cfields . fst) cons
                     conNames = map (cname . fst) cons
                     Just cf = lookup con (zip conNames conFields)
                     fields
                         = case cf of
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
    { withVersion = const id
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
                0 -> return ()
                1 ->
                    do writeSmart sFormatUnvers (head ar)
                       el <- lift get
                       lift $ put $ array [el]
                n ->
                    do accArray [] ar (writeSmart sFormatUnvers)
                       ar <- lift get
                       lift $ put $ arConcat ar
    , writeInt =
          \prim ->
              case prim of
                PrimInt i ->
                    do lift $ put $ Json.Number $ fromIntegral i
                       return ()
                _ -> mismatch "Prim Int" (show prim)
    , writeInteger =
          \prim ->
              case prim of
                PrimInteger i ->
                    do lift $ put $ Json.Number $ fromIntegral i
                       return ()
                _ -> mismatch "Prim Integer" (show prim)
    , writeChar =
          \prim ->
              case prim of
                PrimChar c ->
                    do lift $ put $ Json.String $ T.pack [c]
                       return ()
                _ -> mismatch "Prim Char" (show prim)
    , writeBool =
          \prim ->
              case prim of
                PrimBool b ->
                    do lift $ put $ Json.Bool b
                       return ()
                _ -> mismatch "Prim Bool" (show prim)
    , writeString =
          \prim ->
              case prim of
                PrimString s ->
                    do lift $ put $ Json.String $ T.pack s
                       return ()
                _ -> mismatch "Prim String" (show prim)
    , writeDouble =
          \prim ->
              case prim of
                PrimDouble d ->
                    do lift $ put $ Json.Number $ fromFloatDigits d
                       return ()
                _ -> mismatch "Prim Double" (show prim)
    , writeVersion = \_ -> return ()
    , writeMaybe =
          \ma ->
          case ma of
            Just a -> writeSmart sFormatUnvers a
            Nothing ->
                do lift $ put Json.Null
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

pFormatUnvers :: ParseFormat (FailT (ReaderT Json.Value (State [String])))
pFormatUnvers
    = ParseFormat
    { readVersioned = id
    , readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case length cons of
                 0 -> noCons
                 1 -> case val of
                        obj@(Json.Object _) ->
                            do let con = head conNames
                                   parser = head parsers
                               _ <- putFieldsFromObj con cons
                               local (const obj) parser
                        ar@(Json.Array _) ->
                            do _ <- putFieldsFromArr ar
                               local (const ar) (head parsers)
                        otherPrim ->
                            case cfields $ fst $ head cons of
                              NF 0 ->
                                  do let parser = snd $ head cons
                                     local (const otherPrim) parser
                              NF 1 ->
                                  do let parser = snd $ head cons
                                     local (const otherPrim) parser
                              _      -> fail "Parsing failure. Was expecting\ 
                                             \ a single-field constructor."
                 _ ->
                    case val of
                      Json.Object obj ->
                          case M.member (T.pack "tag") obj of
                            True -> do
                                let Just (Json.String con) = M.lookup (T.pack "tag") obj
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
                            f -> mismatch "tagged type" (show obj)
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
                      _ ->
                          mismatch "tagged type" (show val)
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
                                Json.Array a  ->
                                     do res <- local (array . drop num . fromArray) ma
                                        put $ tail xs
                                        return res
                                n ->
                                     do res <- local (const n) ma
                                        put $ tail xs
                                        return res
                                        
                       [] ->
                           do Json.Object _ <- ask
                              let field = T.pack $ head xs
                              res <- local (fromJust . lookup field . fromObject) ma
                              put $ tail xs
                              return res
                        
    , readRepetition =
            do val <- ask
               case val of
                 Json.Array ar ->
                     case V.toList ar of
                       ar1@(Json.Array _):_ ->
                           local (const ar1) (readRepetition pFormatUnvers)
                       _ ->
                           forM (V.toList ar) (\el -> local (const el) $
                           readSmart pFormatUnvers)
                 _ -> mismatch "Array" (show val)
    , readInt =
        do x <- ask
           case x of
             Json.Number n ->
                  return $ PrimInt $ floor n
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number n:xs -> return $ PrimInt $ floor n
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    
    , readChar =
        do x <- ask
           case x of
             Json.String s ->
                  let str = T.unpack s in
                  if length str == 1
                     then return $ PrimChar $ head str
                     else mismatch "Char" (T.unpack s)
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs ->
                          let str = T.unpack s in
                          if length str == 1
                             then return $ PrimChar $ head str
                             else mismatch "Char" (T.unpack s)
                      _ -> mismatch "Char" (show x)
             _ -> mismatch "Char" (show x)
    , readBool =
        do x <- ask
           case x of
             Json.Bool b -> return $ PrimBool b
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Bool b:xs -> return $ PrimBool b
                      _ -> mismatch "Bool" (show x)
             _ -> mismatch "Bool" (show x)
    , readDouble =
        do x <- ask
           case x of
             Json.Number d -> return $ PrimDouble $ realToFrac d
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.Number d:xs -> return $ PrimDouble $ realToFrac d
                      _ -> mismatch "Number" (show x)
             _ -> mismatch "Number" (show x)
    , readString =
        do x <- ask
           case x of
             Json.String s -> return $ PrimString $ T.unpack s
             ar@(Json.Array _) ->
                    case fromArray ar of
                      Json.String s:xs -> return $ PrimString $ T.unpack s
                      _ -> mismatch "String" (show x)
             _ -> mismatch "String" (show x)
    , readVersion = return Nothing
    , readMaybe =
        do x <- ask
           case x of
             Json.Null -> return Nothing
             ar@(Json.Array _) ->
                case fromArray ar of
                  Json.Null:xs -> return Nothing
                  xs -> liftM Just $ readSmart pFormatUnvers 
             xs -> liftM Just $ readSmart pFormatUnvers
        }
    where putFieldsFromObj con cons = 
              do let conFields = map (cfields . fst) cons
                     conNames = map (cname . fst) cons
                     Just cf = lookup con (zip conNames conFields)
                     fields
                         = case cf of
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
          return ()

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
