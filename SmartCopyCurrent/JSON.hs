{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module JSON where

import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Writer
import Data.Aeson.Encode (fromValue)
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
import Instances
import SmartCopy

encode :: Json.Value -> LBS.ByteString
encode = encodeUtf8 . toLazyText . fromValue

jsonSerializationFormat :: SerializationFormat (Writer Json.Value) Json.Value
jsonSerializationFormat
    = SerializationFormat
    { runSerialization =
          \m -> snd $ runWriter m
    , beginWritingCons =
          \cons ->
           case (fst . snd) cons of
             False -> 
                case (snd . snd) cons of
                  False ->
                    tell $ Json.Array $ V.empty
                  True ->
                    tell $ Json.object [(fst cons, Json.Null)]
             True ->
                tell $ Json.object [("tag", Json.String $ fst cons),
                                       ("contents", Json.Array $ V.empty)]
               -- do tell $ Json.object [(fst cons, Json.Array $ V.empty)]
    , writePrimitive =
          \prim ->
            case prim of
              PrimNum i -> tell $ Json.Number $ fromIntegral i
              PrimBool b -> tell $ Json.Bool b
              _            -> fail "No primitive value"
    , endWritingCons = tell Json.Null
    }

jsonParseFormat :: ParseFormat Json.Value (FailT (Reader Json.Value))
jsonParseFormat
    = ParseFormat
    { runParser = \action value -> runReader (runFailT action) value
    , readCustom =
        \cons ->
            do Json.Object obj <- ask
               let conNames = map (fst . fst) cons
                   parsers = map snd cons
               let [(con, args)] = M.toList obj
               case lookup con (zip conNames parsers) of
                 Just parser -> local (const args) parser
                 Nothing     -> fail $ "Didn't find constructor for tag " ++ show con ++ ". Only found " ++ show conNames
    , readField =
        \i cons ->
            do Json.Object obj <- local (Json.object . (: []) .(!! i) . fromObject) ask
               let conNames = map (fst . fst) cons
                   parsers = map snd cons
               let [(con, args)] = M.toList obj
               case lookup con (zip conNames parsers) of
                 Just parser -> local (const args) parser
                 Nothing    -> fail $ "Didn't find selector for tag " ++ show con ++ ". Only found " ++ show conNames 
    , readNum =
        do x <- ask
           case x of
             Json.Number n -> return $ floor n
             f             -> fail $ "Parsing error. Was expecting number, but found: " ++ show f
    , readBool =
        do x <- ask
           case x of
             Json.Bool b -> return b
             f           -> fail $ "Parsing error. Was expecting number, but found: " ++ show f
    }


fromObject (Json.Object o) = M.toList o

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
            [(a, Json.Array ar)] ->
                Json.object [(a, Json.Array $ ar `V.snoc` v)]
            [(a, Json.Null)] ->
                Json.object [(a, v)]

    Json.Array ar1 `mappend` Json.Array ar2
        = Json.Array $ ar1 V.++ ar2
    Json.Array ar1 `mappend` v
        = Json.Array $ ar1 `V.snoc` v
    v1 `mappend` v2
        = Json.Array $ V.fromList [v1, v2]

