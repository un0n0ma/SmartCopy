{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module JSON where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances
import SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------

import Data.Aeson.Encode (fromValue)
import Data.Aeson.Utils (fromFloatDigits)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as JT
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
import Data.Maybe


encode :: Json.Value -> LBS.ByteString
encode = encodeUtf8 . toLazyText . fromValue

--- Run functions, versioned and unversioned

serializeSmart a = runSerialization (smartPut jsonSerializationFormat a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseSmart :: SmartCopy a => Json.Value -> Fail a
parseSmart = runParser (smartGet jsonParseFormat)
    where runParser action value = evalState (runReaderT (runFailT action) value) []

serializeUnversioned a = runSerialization (writeSmart jsonSerializationFormatUnvers a)
    where runSerialization m = execState (evalStateT m (Left Json.Null)) Json.Null

parseUnversioned :: SmartCopy a => Json.Value -> Fail a
parseUnversioned = runParser (readSmart jsonParseFormatUnvers)
    where runParser action value = evalState (runReaderT (runFailT action) value) []

--- Formats, unversioned and versioned

jsonSerializationFormat
    = jsonSerializationFormatUnvers
    { withVersion =
          \ver ma ->
          do ma
             val <- lift get
             writeVersion jsonSerializationFormat ver
             vers <- lift get
             lift $ put $ Json.object $ fromObject vers ++ fromObject val
    , writeVersion =
          \ver ->
          do let version = fromIntegral $ unVersion ver
             lift $ put $ Json.object [("version", Json.Number version)]
    , withRepetition =
          \ar ->
              case length ar of
                0 -> return ()
                n -> do putter <- getSmartPut jsonSerializationFormat
                        accArray [] ar putter
                        ar <- lift get
                        lift $ put $ arConcat ar
    }

jsonParseFormat
    = jsonParseFormatUnvers
    { readVersioned =
        \ma ->
        do val <- ask
           case val of
             Json.Object obj ->
                if M.member (T.pack "version") obj
                   then if M.member (T.pack "object") obj
                        then do let Just withoutVersion = M.lookup (T.pack "object") obj
                                local (const $ withoutVersion) ma
                        else do let withoutVersion = M.delete (T.pack "version") obj
                                local (const $ Json.Object withoutVersion) ma
                   else ma
             Json.Array ar ->
                do wv <- forM (V.toList ar) withoutVersion
                   local (const $ array wv) ma
             _ -> ma
                
    , readVersion =
        do val <- ask
           case val of
             Json.Object obj -> getVersionFromObj obj
             Json.Array ar ->
                case V.length ar of
                  0 -> mismatch "fields in array" (show val)
                  _ -> do
                      case V.head ar of
                        Json.Object obj -> getVersionFromObj obj
                        _ -> return $ Version 0
             val -> return $ Version 0
    , readRepetition =
            do val <- ask
               case val of
                 Json.Array ar ->
                     forM (V.toList ar) (\el -> local (const el) (readSmart jsonParseFormat))
                 o@(Json.Object _) ->
                     readVersioned jsonParseFormat $ readRepetition jsonParseFormatUnvers
                 _ -> mismatch "Array" (show val)
              
    }
    where withoutVersion o@(Json.Object obj) =
               if M.member (T.pack "version") obj
                  then if M.member (T.pack "object") obj
                       then return $ fromJust $ M.lookup (T.pack "object") obj
                       else return $ Json.Object (M.delete (T.pack "version") obj)
                  else return o
          withoutVersion val = return val
          getVersionFromObj obj =
              if M.member (T.pack "version") obj
                 then do let Just v@(Json.Number version) = M.lookup (T.pack "version") obj
                         return $ Version $ floor version
                 else return $ Version 0


jsonSerializationFormatUnvers :: SerializationFormat (StateT (Either Json.Value [JT.Pair]) (State Json.Value))
jsonSerializationFormatUnvers
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

    , withRepetition =
          \ar ->
              case length ar of
                0 -> return ()
                n -> do accArray [] ar (writeSmart jsonSerializationFormatUnvers)
                        ar <- lift get
                        lift $ put $ arConcat ar
    , writeInt =
          \prim ->
              case prim of
                PrimInt i ->
                    do lift $ put $ Json.Number $ fromIntegral i
                       return ()
                _ -> mismatch "Prim Int" (show prim)
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
    , writeVersion = undefined
    }


jsonParseFormatUnvers :: ParseFormat (FailT (ReaderT Json.Value (State [String])))
jsonParseFormatUnvers
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
                                            fail $ msg (T.unpack con) conNames
                                  Nothing ->
                                      do let args = M.delete (T.pack "tag") obj
                                         case lookup con (zip conNames parsers) of
                                           Just parser ->
                                               do _ <- putFieldsFromObj con cons
                                                  local (const $ object args) parser
                                           Nothing ->
                                               fail $ msg (T.unpack con) conNames
                            f -> mismatch "tagged type" (show obj)
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const o) (readCons jsonParseFormat cons)
                            nameOrField@(Json.String _):_ ->
                                local (const nameOrField) (head parsers)
                            f -> mismatch "tagged type" (show f)
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
                     forM (V.toList ar) (\el -> local (const el) (readSmart jsonParseFormatUnvers))
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
    , readVersion = return $ Version 1
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
                 return $ T.unpack ""
          putFieldsFromArr ar =
              do let l = length $ fromArray ar
                     fields = [0..l-1]
                 put $ map show fields
                 return $ T.unpack ""
          msg con cons = "Didn't find constructor for tag " ++ con ++ "Only found " ++ show cons


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
          let val
                  = case val' of
                      Json.Array ar -> V.toList ar
                      p -> [p]
          let acc = xs ++ val
          lift $ put $ array (xs ++ val)
          accArray acc (tail ar) wf
          return ()
takeEmptyField [] notnull =
    fail "Encoding failure. Got more fields than expected for constructor."
takeEmptyField map notnull =
       case head map of
         f@(_, Json.Null) -> return (f, notnull ++ tail map)
         x -> takeEmptyField (tail map) (x:notnull)
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
