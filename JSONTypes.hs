{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module JSONTypes where

import Data.Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import SmartCopy as SC
import Data.Typeable (Typeable)

-------------------------------------------------------------------------------
-- JSON Values
-------------------------------------------------------------------------------

emptyObject :: Value
emptyObject = Object M.empty

emptyArray :: Value
emptyArray = Array V.empty

array :: [Value] -> Value
array vs = Array $ V.fromList vs

-------------------------------------------------------------------------------
-- Monad
-------------------------------------------------------------------------------

newtype JSVal a = JSVal { unJSVal :: Maybe Value }
newtype JSON a = JSON { unJSON :: Value } --- we need something like:
 
instance Functor JSON where
    fmap = undefined

instance FormatMonad JSON where
    type EncodedType = Value
    unPack ma = unJSON ma
    returnEmpty = JSON emptyObject
    enterCons (Cons (PrimString name) tagOrNot) v =
        case (unPack v) of
          Object _ ->
            case tagOrNot of
              False -> v
              True ->
                  let t = String $ T.pack name
                      cont = unPack v
                  in JSON $ object [("tag", t), ("contents", cont)]
          _ -> returnEmpty
    enterField (Field i s) v =
        let name =
              case s of
                ""      -> T.pack $ show i
                _ -> T.pack s
        in
        JSON $ object [(name, unPack v)]
    openRepetition length vs =
        JSON $ array (map unPack vs)
    writeValue v =
        let val =
              case v of
                PrimInt i    -> Number $ fromInteger i
                PrimString s -> String $ T.pack s
                PrimChar c   -> String $ T.pack [c]
        in JSON val
    closeRepetition = JSON emptyArray
    mult j1 j2 = let (Object v1) = unPack j1
                     (Object v2) = unPack j2
                     vals = object $ M.toList $ M.union v1 v2
                 in JSON vals
