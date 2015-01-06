{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module JSONTypes where

import Data.Map as M
import Data.Text as T
import Data.Vector as V
import SmartCopyTest
import Data.Typeable (Typeable)

data Value = Object !Object
           | Array !Array
           | String !T.Text
           | Number !Number
           | Bool !Bool
           | Null
           deriving (Eq, Show, Typeable)

type Object = Map T.Text Value

type Array = Vector Value

type Number = Int

emptyObject :: Value
emptyObject = Object M.empty

emptyArray :: Value
emptyArray = Array V.empty

type KVPair = (T.Text, Value)

object :: [KVPair] -> Value
object = Object . M.fromList

newtype JSON a = JSON { unJSON :: Value }

instance FormatMonad JSON where
    type ReturnType = Value
    unPack ma = unJSON ma
    returnCons a = JSON emptyObject
    returnField (Field i ms) =
        let name =
              case ms of
                Just s -> T.pack s
                _      -> T.pack $ show i
        in
        JSON $ object [(name, Null)]
    returnValue v =
        let val =
              case v of
                PrimInt i    -> Number i
                PrimString s -> String $ T.pack s
                PrimChar c   -> String $ T.pack $ [c]
        in JSON val

jsonFormat :: Format JSON Value
jsonFormat = undefined
    { fmt_enterDataCon = \c -> returnCons c
    , fmt_enterField = \f -> returnField f
    , fmt_writeValue = \p -> returnValue p
    , fmt_leaveField = undefined
    , fmt_leaveDataCon = undefined
    }

