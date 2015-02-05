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

import Control.Applicative
import Data.Maybe


encode :: Json.Value -> LBS.ByteString
encode = encodeUtf8 . toLazyText . fromValue

jsonSerializationFormat :: SerializationFormat (Writer Json.Value) Json.Value
jsonSerializationFormat
    = SerializationFormat
    { runSerialization =
          \m -> do snd $ runWriter m
    , withCons =
          \cons ma ->
          case ctagged cons of
            False ->
                case cfields cons of
                  Left 0 ->
                    do tell $ Json.String $ cname cons
                  Left 1 ->
                       ma
                  Left n ->
                    do tell $ Json.Array V.empty
                       ma
                  Right n ->
                    do tell $ Json.object $ [(cname cons, Json.Null)]
                       ma
            True ->
                case cfields cons of
                  Left n ->
                    do tell $ Json.object [("tag", Json.String $ cname cons),
                                           ("contents", Json.Array $ V.empty)]
                       ma
                  Right n ->
                    do tell $ Json.object [("tag", Json.String $ cname cons)]
                       ma
    , withField =
          \nameOrIndex ma ->
              case nameOrIndex of
                Index _ -> ma
                Labeled label ->
                    do tell $ Json.object [(label, Json.Null)]
                       ma
    , withRepetition =
          \wf ar ->
            case length ar of
              0 -> do tell $ array []
              n -> do tell $ array []
                      sequence_ $ map wf ar
    , writePrimitive =
          \prim ->
            case prim of
              PrimInt i -> tell $ Json.Number $ fromIntegral i
              PrimBool b -> tell $ Json.Bool b
              PrimString s -> tell $ Json.String $ T.pack s
              PrimDouble d ->
                  tell $ Json.Number $ fromFloatDigits d
    }

jsonParseFormat :: ParseFormat Json.Value (FailT (Reader Json.Value))
jsonParseFormat
    = ParseFormat
    { runParser = \action value -> runReader (runFailT action) value
    , readCons =
        \cons ->
            do val <- ask
               let conNames = map (cname . fst) cons
                   parsers = map snd cons
               case length cons of
                 0 -> fail "Parsing failure. No constructor to look up."
                 1 -> case val of
                        Json.Object obj ->
                            do let [(con, args)] = M.toList obj
                               case lookup con (zip conNames parsers) of
                                 Just parser -> local (const args) parser
                                 Nothing ->
                                    fail $ msg (T.unpack con) conNames
                            where msg con cons = "Didn't find constructor for tag " ++ con ++
                                                 "Only found " ++ show cons
                        ar@(Json.Array _) ->
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
                          case (cfields $ fst $ head cons) of
                            Left _ ->
                              do let [("tag", Json.String con), ("contents", args)] = M.toList obj
                                 case lookup con (zip conNames parsers) of
                                   Just parser -> local (const args) parser
                                   Nothing ->
                                     fail $
                                     "Didn't find constructor for tag " ++ (T.unpack con) 
                                     ++ "Only found " ++ show conNames
                            {- FIX THIS
                            Right _ ->
                              do let [("tag", Json.String con), args] = M.toList obj
                                 case lookup con (zip conNames parsers) of
                                   Just parser -> local (const args) parser
                                   Nothing ->
                                     fail $
                                     "Didn't find constructor for tag " ++ (T.unpack con) 
                                     ++ "Only found " ++ show conNames
                                     -}
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const o) (readCons jsonParseFormat cons)
                            nameOrField@(Json.String _):_ ->
                            -- Needed for SumTypes with no fields, e.g
                            -- MyBool = MyTrue | MyFalse. Types are not tagged
                            -- in [("tag",...),("contents",...)] form, but have
                            -- multiple constructors for lookup.
                                local (const nameOrField) (head parsers)
                            f ->
                                fail $ "Parsing failure. Was expecting a tagged type.\
                                       \ found" ++ show f
                      _ ->
                          fail "Parsing failure. Was expecting a tagged type."

                        
    , readField =
        \nameOrIndex ma ->
            case nameOrIndex of
              Index index ->
                  do v <- ask
                     case v of
                       Json.Array a  ->
                            local (array . drop index . fromArray) ma
                       n -> local (const n) ma
              Labeled l ->
                 do Json.Object _ <- ask
                    local (fromJust . (lookup l) . fromObject) ma
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

instance Monoid Json.Value where
    mempty = Json.Null
    Json.Null `mappend` v
        = v
    v `mappend` Json.Null
        = v
    Json.Object o1 `mappend` v =
        case M.toList o1 of
          [k@("tag", _), ("contents", Json.Array ar1)] ->
              case v of
                  Json.Array ar2 ->
                      Json.object [k, ("contents", Json.Array $ ar1 V.++ ar2)]
                  _ ->
                      Json.object [k, ("contents", Json.Array $ ar1 `V.snoc` v)]
          [k@("tag", _), ("contents", o1@(Json.Object _))] ->
              case v of
                o2@(Json.Object _) ->
                  Json.object [k, ("contents", Json.object $ (fromObject o1) ++ (fromObject o2))]
                val ->
                  case fromObject o1 of
                    [(tag, Json.Null)] ->
                        Json.object [k, ("contents", Json.object $ fromObject o1 ++ [(tag, val)])]
          [k@("tag", _), _] ->
              case v of
                o2@(Json.Object _) ->
                  Json.object $ [k] ++ fromObject o2
          [(a, Json.Array ar)] ->
            Json.object [(a, Json.Array $ ar `V.snoc` v)]
          [(a, Json.Null)] ->
              Json.object $ [(a, v)]
          o1@[(a, primVal)] ->
              case v of
                o2@(Json.Object _) ->
                    Json.object $ o1 ++ (fromObject o2)
    Json.Array ar1 `mappend` Json.Array ar2
        = Json.Array $ ar1 V.++ ar2
    Json.Array ar1 `mappend` v
        = Json.Array $ ar1 `V.snoc` v
    v `mappend` Json.Array ar1
        = Json.Array $ v `V.cons` ar1
    v1 `mappend` v2
        = Json.Array $ V.fromList [v1, v2]

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


fromObject (Json.Object o) = M.toList o
fromArray (Json.Array a) = V.toList a
array = Json.Array . V.fromList

