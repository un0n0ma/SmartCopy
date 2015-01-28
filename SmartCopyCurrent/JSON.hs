{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module JSON where

import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import Data.Aeson.Encode (fromValue)
import Data.Maybe
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Aeson as Json
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

-------------------------------------------------------------------------------
-- Local
-------------------------------------------------------------------------------
import MonadTypesInstances
import SmartCopy


encode :: Json.Value -> LBS.ByteString
encode = encodeUtf8 . toLazyText . fromValue

jsonSerializationFormat :: SerializationFormat (Writer Json.Value) Json.Value
jsonSerializationFormat
    = SerializationFormat
    { runSerialization =
          \m -> arConcat $ snd $ runWriter m
    , beginWritingCons =
          \cons ->
          case ctagged cons of
            False ->
                case cfields cons of
                  Left 0 ->
                    do tell $ Json.String $ cname cons
                  Left 1 ->
                    do tell $ Json.Null
                  Left _ ->
                    do tell $ Json.Array $ V.empty
                  Right _ ->
                    do tell $ Json.object $ [(cname cons, Json.Null)]
            True ->
                case cfields cons of
                  Left _ ->
                    do tell $ Json.object [("tag", Json.String $ cname cons),
                                        ("contents", Json.Array $ V.empty)]
                  Right _ ->
                    do tell $ Json.object [("tag", Json.String $ cname cons),
                                        ("contents", Json.Object $ M.empty)]
    , withField =
          \nameOrIndex ma ->
              case nameOrIndex of
                Left index -> ma
                Right label -> do tell $ Json.object [(label, Json.Null)]
                                  ma
    , writePrimitive =
          \prim ->
            case prim of
              PrimInt i -> tell $ Json.Number $ fromIntegral i
              PrimBool b -> tell $ Json.Bool b
              _          -> fail "No primitive value"
    , endWritingCons = tell Json.Null
    }

jsonParseFormat :: ParseFormat Json.Value (FailT (Reader Json.Value))
jsonParseFormat
    = ParseFormat
    { runParser = \action value -> runReader (runFailT action) value
    , readCustom =
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
                          do let [("tag", Json.String con), ("contents", args)] = M.toList obj
                             case lookup con (zip conNames parsers) of
                               Just parser -> local (const args) parser
                               Nothing -> fail $ msg (T.unpack con) conNames
                          where msg con cons = "Didn't find constructor for tag " ++ con ++
                                                "Only found " ++ show cons
                      ar@(Json.Array _) ->
                          case fromArray ar of
                            o@(Json.Object _):_ ->
                                local (const o) (readCustom jsonParseFormat cons)
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
              Left index ->
                  do v <- ask
                     case v of
                       Json.Array a  ->
                            local (array . drop index . fromArray) ma
                       n -> local (const n) ma
              Right label ->
                 do Json.Object _ <- ask
                    local (fromJust . (lookup label) . fromObject) ma
    , readNum =
        do x <- ask
           case x of
             Json.Number n -> return $ floor n
             ar@(Json.Array _) -> do
                    case fromArray ar of
                      Json.Number n:xs -> return $ floor n


             f             -> fail $ "Parsing error. Was expecting number, but found: " ++ show f
    , readBool =
        do x <- ask
           case x of
             Json.Bool b -> return b
             f           -> fail $ "Parsing error. Was expecting number, but found: " ++ show f
    }



instance Monoid Json.Value where
    mempty = Json.Null
    Json.Object o1 `mappend` v
        = case M.toList o1 of
            [k@("tag", _), ("contents", Json.Array ar)] ->
                case v of
                  Json.Array ar2 ->
                    Json.object [k, ("contents", Json.Array $ ar V.++ ar2)]
                  _              ->
                    Json.object [k, ("contents", Json.Array $ ar `V.snoc` v)]
            [k@("tag", _), ("contents", o1@(Json.Object _))] ->
                case v of
                  o2@(Json.Object _) ->
                    Json.object [k, ("contents", Json.object $ (fromObject o1) ++ (fromObject o2))]
                  val ->
                    case fromObject o1 of
                      [(tag, Json.Null)] ->
                        Json.object [k, ("contents", Json.object $ [(tag, val)])]
                       

            [(a, Json.Array ar)] ->
                Json.object [(a, Json.Array $ ar `V.snoc` v)]
            [(a, Json.Null)] ->
                Json.object [(a, v)]
            o1@[(a, primVal)] ->
                case v of
                  o2@(Json.Object _) ->
                      Json.object $ o1 ++ (fromObject o2)

    Json.Array ar1 `mappend` Json.Array ar2
        = Json.Array $ ar1 V.++ ar2
    Json.Array ar1 `mappend` v
        = Json.Array $ ar1 `V.snoc` v
    Json.Null `mappend` v
        = v
    v1 `mappend` v2
        = Json.Array $ V.fromList [v1, v2]

arConcat :: Json.Value -> Json.Value
arConcat a@(Json.Array ar)
    = let vs = V.toList ar in
      case length vs of
        1 -> head vs
        _ -> a
arConcat o = o


fromObject (Json.Object o) = M.toList o
fromArray (Json.Array a) = V.toList a
array = Json.Array . V.fromList

