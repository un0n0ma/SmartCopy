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

serializeSmart a = runSerialization (writeSmart jsonSerializationFormat a)
    where runSerialization m = snd $ runState (evalStateT m (Left Json.Null)) Json.Null

parseSmart :: SmartCopy a => Json.Value -> Fail a
parseSmart = runParser (readSmart jsonParseFormat)
    where runParser action value = evalState (runReaderT (runFailT action) value) []


jsonSerializationFormat :: SerializationFormat (StateT (Either Json.Value [JT.Pair]) (State Json.Value))
jsonSerializationFormat
    = SerializationFormat
    { withCons =
          \cons ma ->
          case ctagged cons of
            False ->
                case cfields cons of
                  Left 0 ->
                    lift $ put $ Json.String $ cname cons
                  Left n ->
                    do put $ Left $ Json.Array V.empty
                       _ <- ma
                       Left res <- get
                       lift $ put $ arConcat res
                  Right ls ->
                    do let fields = Json.object (zip ls (repeat Json.Null))
                       lift $ put fields
                       _ <- ma
                       res <- lift get
                       lift $ put res
            True ->
                case cfields cons of
                  Left 0 ->
                    lift $ put $ Json.object [("tag", Json.String $ cname cons),
                                       ("contents", Json.Array V.empty)]
                  Left n ->
                    do put $ Left $ Json.Array V.empty
                       _ <- ma
                       Left res <- get
                       let resObj = Json.object [("tag", Json.String $ cname cons),
                                                 ("contents", arConcat res)]
                       lift $ put resObj
                  Right ls ->
                    do put $ Right $ zip ls (repeat Json.Null)
                       _ <- ma
                       Right res <- get
                       let resObj 
                            = Json.object $ ("tag", Json.String $ cname cons):res
                       lift $ put resObj
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
                   f -> fail $ show f

    , withRepetition =
          \wf ar ->
            case length ar of
              0 -> return ()
              n -> do accArray [] ar wf
                      ar <- lift get
                      lift $ put $ arConcat ar

    , writePrimitive =
          \prim ->
            case prim of
              PrimInt i ->
                  do lift $ put $ Json.Number $ fromIntegral i
                     return ()
              PrimBool b ->
                  do lift $ put $ Json.Bool b
                     return ()
              PrimString s ->
                  do lift $ put $ Json.String $ T.pack s
                     return ()
              PrimDouble d ->
                  do lift $ put $ Json.Number $ fromFloatDigits d
                     return ()
    }
    where accArray xs [] wf = return ()
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
              fail $ "Encoding failure. Got more fields than expected for constructor."
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

jsonParseFormat :: ParseFormat (FailT (ReaderT Json.Value (State [String])))
jsonParseFormat
    = ParseFormat
    { readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
                   conFields = map (cfields . fst) cons
               case length cons of
                 0 -> fail "Parsing failure. No constructor to look up."
                 1 -> case val of
                        Json.Object obj ->
                            do let [(con, args)] = M.toList obj
                               case lookup con (zip conNames parsers) of
                                 Just parser ->
                                    do _ <- putFieldsFromObj con cons                                  
                                       local (const args) parser
                                 Nothing ->
                                    fail $ msg (T.unpack con) conNames
                        ar@(Json.Array _) ->
                            do _ <- putFieldsFromArr ar
                               local (const ar) (head parsers)
                        otherPrim ->
                            case (cfields $ fst $ head cons) of
                              Left 0 ->
                                  do let parser = snd $ head cons
                                     local (const otherPrim) parser
                              Left 1 ->
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
                            f -> fail $ show f
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const o) (readCons jsonParseFormat cons)
                            nameOrField@(Json.String _):_ ->
                                local (const nameOrField) (head parsers)
                            f ->
                                fail $ "Parsing failure. Was expecting a tagged type.\
                                       \ found" ++ show f
                      _ ->
                          fail "Parsing failure. Was expecting a tagged type."

                        
    , readField =
        \ma ->
            do fields <- lift $ lift get
               case fields of
                 [] -> ma
                 xs ->
                     do case reads $ head xs of
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
                                 res <- local (fromJust . (lookup field) . fromObject) ma
                                 put $ tail xs
                                 return res
    , readPrim =
        do x <- ask
           case x of
             Json.Number n ->
                  return $ PrimDouble $ realToFrac n
             Json.Bool b -> return $ PrimBool b
             Json.String s -> return $ PrimString $ T.unpack s
             ar@(Json.Array _) -> do
                    case fromArray ar of
                      Json.Number n:xs -> return $ PrimInt $ floor n
                      Json.Bool b:xs -> return $ PrimBool b
                      Json.String s:xs -> return $ PrimString $ T.unpack s


             f             -> fail $ "Parsing error. Was expecting primitive, but found: " ++ show f
    }
    where putFieldsFromObj con cons = 
              do let conFields = map (cfields . fst) cons
                     conNames = map (cname . fst) cons
                     Just cf = lookup con (zip conNames conFields)
                     fields
                         = case cf of
                             Left i -> map show [0..i-1]
                             Right lbs -> map T.unpack lbs
                 put fields
                 return $ T.unpack ""
          putFieldsFromArr ar =
              do let l = length $ fromArray ar
                     fields = [0..l-1]
                 put $ map show fields
                 return $ T.unpack ""
          msg con cons = "Didn't find constructor for tag " ++ con ++ "Only found " ++ show cons



fromObject (Json.Object o) = M.toList o
fromArray (Json.Array a) = V.toList a
array = Json.Array . V.fromList
object = Json.Object

