

module SafeCopy where

import SmartCopy
import Control.Monad.
import Data.Binary.Pu
t
import Data.ByteString as BS


binarySerializationFormat :: SerializationFormat PutM BS.ByteString
binarySerializationFormat = undefined

binaryParseFormat :: ParseFormat BS.ByteString (FailT (Reader BS.ByteString))
binaryParseFormat = undefined
