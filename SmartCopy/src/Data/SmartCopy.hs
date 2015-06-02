-- |
-- Module      :  Data.SmartCopy
-- Copyright   :  PublicDomain
--
-- Maintainer  : nothdurft@cp-med.com
--
-- smartcopy provides an interface for implementing various (de-)serialization
-- formats and comes with implementations of JSON, Binary, XML and String
-- formats. Data.SafeCopy-style version control is supported and extended with
-- back-migration, which can be useful in distributed system use cases.
module Data.SmartCopy
    ( -- $use
      SmartCopy (..)
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

-- $use
--
-- smartcopy currently provides 4 base format implementations:
-- JSON, binary (corresponding to cereal/safecopy), XML and a String encoding
-- similar to show.
-- To serialize a data type using one of these formats, the library user
-- declares just one SmartCopy instance of the data type. Writing instances
-- manually cumbersome in the current implementation. For reference,
-- see TestInstances.hs in the Tests directory.
-- Since a GHC.Generics implementation exists, instances can simply be
-- derived by leaving the instance body empty.
--
-- smartcopy supports nested version control in the tradition of
-- Data.SafeCopy, using the same migration mechanism.
-- To extend a data type with a newer version, a Migrate instance has 
-- to be declared, defining a migration function from old to new.
-- Migrations have to be written manually. Examples can be found in
-- TestInstancesMigrate.hs in the Tests directory. More detailed
-- information is available in the safecopy documentation.
--
-- Versioned variants exist of all implemented formats and have separate
-- run functions. In the provided formats, the serializeSmart/parseSmart
-- functions support nested version control, the serializeUnvers/parseUnvers
-- functions deal with unversioned data. In addition, the serializeLastKnown/
-- parseLastKnown functions handle data that has to be guaranteed to be
-- backwards compatible for exchange with older system components.
-- Not all formats define parseLastKnown, since a separate parsing format
-- is only required when the encoding per default represents constructor
-- names, which is mostly the case in text-based formats.
--
-- Defining a new format is more complicated. Unversioned formats
-- are the simplest to define, since they can ignore a lot of parameters,
-- but the format interface can still be confusing, so it's adviced to
-- look at the format implementations for concrete examples.
-- The format interface consists of two record types
-- data SerializationFormat m
-- and
-- data ParseFormat m
-- that are parametrized over a monad. The monad of choice depends on the
-- specific demands of the format. In general, binary formats are expected
-- to get along with Put and Get monads. A format needing more information
-- and differing from safecopy's sequential structure, will likels
-- neeed a combination of transformers to encode state.
--
-- The format records define functions that handle base types and functions
-- specifying constructor and field encoding.
-- The functions mkPutter/mkGetter specify exactly how versions are handled
-- in a format.
-- There are two main ways:
-- - Write/read a version tag as a side effect and return a putter(getter)
--   constructed from the version. This corresponds to safecopy's default
--   and is adopted in our safecopy and String format implementation.
-- - Return a putter(getter) that writes(reads) both version tag and contents:
--   This is the case in our JSON and XML implementations that deviate
--   from safecopy's strictly sequential structure and incorporate version
--   tags in composite data structures
-- mkPutter/mkGetter are trivial for unversioned formats.
--
-- Currently, the polymorphic functions defined in the format records
-- have an additional Maybe [String] parameter, that can be completely
-- disregarded in all formats that don't consider back-compatibility.
-- When defining a format that should enable back-migration,
-- the functions have to be called with a list containing identifier names
-- of all SmartCopy instances known in a communicating component.
-- Identifiers are defined in the SmartCopy class.
-- There is no given naming convention, but it should be ensured that
-- identifiers are unique and unchanging (independent of type evolution).
-- A suggestion is to use a string consisting of data type name
-- (in its most current version) and version.
-- The system then compares the identifier list with the value to be
-- serialized, if known writing it in the current version, or otherwise
-- migrating back to its predecessor, repeating the look-up.
-- This should guarantee that a data type is never serialized in a version
-- to recent to be interpreted by a component having an older data model.
