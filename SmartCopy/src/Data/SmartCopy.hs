module Data.SmartCopy
    ( SmartCopy (..)
    , Migrate (..)
    , SerializationFormat (..)
    , ParseFormat (..)
    , ConstrInfo (..)
    , Fields (..)
    , smartPut
    , smartGet
    , smartPutLastKnown
    , getSmartGet
    , getSmartPut
    , getSmartPutLastKnown
    , base
    , extension
    , primitive
    , Version (..)
    , Identifier (..)
    , fromOk
    , Fail (..)
    , fromEitherFail
    )
where

import Data.SmartCopy.Instances ()
import Data.SmartCopy.MonadTypesInstances (fromOk, Fail (..))
import Data.SmartCopy.SmartCopy
