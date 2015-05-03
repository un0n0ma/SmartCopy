{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


-- |
-- Module      :  Data.SmartCopy.SmartCopy
-- Copyright   :  PublicDomain
--
-- Maintainer  :  nothdurft@cp-med.com
--
-- SmartCopy provides an interface for implementing various (de-)serialization
-- formats and comes with implementations of JSON, Binary, XML and String
-- formats. Data.SafeCopy-style version control is supported and extended with
-- back-migration, which can be useful in distributed system use-cases.
module Data.SmartCopy.SmartCopy
       ( SmartCopy (..)
       , GSmartCopy (..)
       , Migrate (..)
       , SerializationFormat (..)
       , ParseFormat (..)
       , getSmartPut
       , getSmartPutLastKnown
       , getSmartGet
       , smartPut
       , smartGet
       , smartPutLastKnown
       , constructGetterFromVersion
       , mismatch
       , noCons
       , conLookupErr
       , noIDListErr
       , idNotFoundPutter
       , ConstrInfo (..)
       , Fields (..)
       , base
       , extension
       , primitive
       , Identifier (..)
       , Version (..)
       , Kind (..)
       , Reverse
       , Proxy (..)
       , versionFromProxy
       , kindFromProxy
       , identFromProxy
       , asProxyType
       , checkConsistency
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy.MonadTypesInstances

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Int (Int32)
import Data.List (nub)
import Data.Text.Internal as T
import Text.Parsec hiding (Empty)

import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import Data.Data hiding (Proxy)
import Data.Typeable hiding (Proxy)
import GHC.Generics
import Unsafe.Coerce

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (MonadTrans(..))
import "mtl" Control.Monad.Writer

-------------------------------------------------------------------------------
-- SmartCopy and Generic SmartCopy type classes
-------------------------------------------------------------------------------

-- |A type that can be serialized in any format implementing the SmartCopy
-- interface.
-- Users of the SmartCopy library should only define the writeSmart and
-- readSmart functions and ignore the smartPut and smartGet functions.
-- For a datatype that doesn't have a preceding version the version and kind
-- information are left default.
-- If a type should explicitly not be tagged with a version its kind can be
-- made primitive.
class SmartCopy a where
    identifier :: Identifier a
    -- |The (unique) version of a datatype. Per default 0.
    version :: Version a
    version = 0
    -- |Specifies how a datatype is versioned. Per default base, meaning
    -- that the version tag is written out when serializing in a versioned
    -- format and that there is no previous version of the type to migrate
    -- from.
    kind :: Kind a
    kind = Base
    -- |Serialize a datatype in an unversioned or versioned serialization format
    -- without worrying about writing out the version tag.
    writeSmart :: Monad m => SerializationFormat m -> a -> Maybe [String] -> m ()
    default writeSmart :: (Generic a, GSmartCopy (Rep a), Monad m)
                       => SerializationFormat m -> a -> Maybe [String] -> m ()
    writeSmart fmt a mIds
        = do wrapped <- gwriteSmart fmt (from a) [] [] [unId (identifier :: Identifier a)] mIds
             putter <- wrapped
             putter (from a)
    -- |Parse a datatype in an unversioned or versioned parse format
    -- without worrying about reading the version tag.
    readSmart :: (Applicative m, Alternative m, Monad m) => ParseFormat m -> m a
    default readSmart ::
                      ( Generic a, GSmartCopy (Rep a), Monad m, Applicative m
                      , Alternative m)
                      => ParseFormat m -> m a
    readSmart fmt
        = do wrapped <- greadSmart fmt [] [] [unId (identifier :: Identifier a)] 
             getter <- wrapped
             fmap to getter

-- |Class that allows to generically convert datatypes to or from a certain
-- format. It is in general not required for the user to write SmartCopy
-- instances, as they can be derived using the DefaultSignatures and
-- DeriveGeneric extensions.
class GSmartCopy t where
    -- |Generic serialization function that can be used with compilers supporting
    -- DeriveGeneric and DefaultSignatures.
    -- Deriving Datatypes are per default serialized in a manner
    -- compatible with TemplateHaskell-generated SafeCopy instances.
    -- As version tags are written out as a side-effect when returning the putter
    -- for a field, deriveSafeCopy-compatibility requires an additional wrapper
    -- around the constructed putter.
    gwriteSmart :: Monad m
                => SerializationFormat m
                -> t x
                -> [ConstrInfo]
                -> [(Maybe TypeRep, Int32)]
                -> [String]
                -> Maybe [String]
                -> m (m (t x -> m ()))
    -- |Generic parsing function that can be used with compilers supporting
    -- DeriveGeneric and DefaultSignatures.
    -- Deriving Datatypes are per default parsed in a manner
    -- compatible with TemplateHaskell-generated SafeCopy instances.
    -- As version tags are read as a side-effect when returning the getter
    -- for a field, deriveSafeCopy-compatibility requires an additional wrapper
    -- around the constructed getter.
    greadSmart :: (Functor m, Applicative m, Monad m, Alternative m)
               => ParseFormat m
               -> [ConstrInfo]
               -> [(Maybe TypeRep, Int32)]
               -> [String]
               -> m (m (m (t x)))

-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

-- |A serialization format contains functions that define how a datatype made
-- an instance of SmartCopy should be converted into a specific output type.
-- Users implementing a new format using the SmartCopy interface are free to
-- choose a monad suited best for the requirements and to implement or
-- disregard concrete version handling.
-- For reference see the thus far implemented formats in Data.SmartCopy.Formats.
data SerializationFormat m
    = SerializationFormat
    { -- |Return the putter for a datatype, writing out the version tag as a
      -- side effect. The SafeCopy implementation per default saves bytes
      -- by writing out only one version tag for multiple occurences of a type
      -- in the fields of a datatype.
      -- For fields that are not the first occurence of a type mkPutter is called
      -- with the first parameter being False, indicating that the version tag
      -- should be saved.
      -- If mkPutter is called with True the version tag is written as side-effect
      -- right after writing the constructor tag.
      -- All so far implemented formats but SafeCopy and String format don't make
      -- this discrimination for duplicate types.
      mkPutter :: SmartCopy a => Bool -> Int32 -> Maybe [String] ->  m (a -> m ())
      -- |Write out constructor information to a datatype as required in the
      -- specified format.
    , withCons :: ConstrInfo -> m () -> m ()
      -- |Write fields of a datatype as required in the specified format. Information
      -- about field labels and indices, if needed to be accessible, can be encoded
      -- in the Monad in the call of withCons.
    , withField :: m () -> m ()
      -- |Write a list according to the specifications of the format. As all list
      -- elements are required to have the same version, the version tag is usually
      -- written out only once for the entire list.
    , writeRepetition :: SmartCopy a => [a] -> Maybe [String] -> m ()
      -- |Serialize an Int primitive.
    , writeInt :: Int -> m ()
      -- |Serialize an Integer primitive.
    , writeInteger :: Integer -> m ()
      -- |Serialize a Char primitive.
    , writeChar :: Char -> m ()
      -- |Serialize a Bool primitive.
    , writeBool :: Bool -> m ()
      -- |Serialize a Double primitive.
    , writeDouble :: Double -> m ()
      -- |Serialize a String primitive.
    , writeString :: String -> m ()
      -- |Serialize a Maybe type.
    , writeMaybe :: SmartCopy a => Maybe a -> Maybe [String] -> m ()
      -- |Serialize a ByteString.
    , writeBS :: BS.ByteString -> m ()
      -- |Serialize a Data.Text.
    , writeText :: T.Text -> m ()
    }

-- |A parse format contains functions that define how a datatype made
-- an instance of SmartCopy should be parsed from a specific input type.
-- Users implementing a new format using the SmartCopy interface are free to
-- choose a monad suited best for the requirements and to implement or
-- disregard concrete version handling.
-- For reference see the thus far implemented formats in Data.SmartCopy.Formats.
data ParseFormat m
    = ParseFormat
    { -- |Reading the version tag as a side effect and return a getter that is
      -- constructed from it. The SafeCopy implementation per default saves bytes
      -- by writing out only one version tag for multiple occurences of a type
      -- in the fields of a datatype.
      -- For fields that are not the first occurence of a type mkGetter is called
      -- with the first parameter being False, indicating that no version tag
      -- should be read, but the getter should instead be constructed from the
      -- second (version) parameter.
      -- If mkGetter is called with True the version tag is read as side-effect
      -- right after reading the constructor tag.
      -- All so far implemented formats but SafeCopy and String format don't make
      -- this discrimination for duplicate types.
      mkGetter :: SmartCopy a => Bool -> Int32 -> Maybe Int32 -> m (m a)
      -- |Read constructor information and parse a datatype accordingly.
      -- readCons is called with a map that contains a parser for each possible
      -- constructor. The ConstrInfo record type has information required for a
      -- lookup in various formats, like name or tag information.
    , readCons :: forall a. [(ConstrInfo, m a)] -> m a
      -- |Read fields of a datatype as required in the specified format. Information
      -- about field labels and indices, if needed to be accessible, can be encoded
      -- in the Monad in the call of withCons.
    , readField :: forall a. m a -> m a
      -- |Read a list according to the specifications of the format. As all list
      -- elements are required to have the same version, only one version tag needs
      -- to be written out and read for the entire list to construct a getter.
    , readRepetition :: SmartCopy a => m [a]
      -- |Read an Int primitive.
    , readInt :: m Int
      -- |Read a Char primitive.
    , readChar :: m Char
      -- |Read a Bool primitive.
    , readBool :: m Bool
      -- |Read a Double primitive.
    , readDouble :: m Double
      -- |Read a String primitive.
    , readString :: m String
      -- |Read a Maybe type.
    , readMaybe :: SmartCopy a => m (Maybe a)
      -- |Read a ByteString.
    , readBS :: m BS.ByteString
      -- |Read a Data.Text.
    , readText :: m T.Text
    }


-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

-- |General error thrown when unexpected data is read during parsing.
mismatch :: Monad m => forall a. String -> String -> m a
mismatch exp act = fail $ "Was expecting " ++ exp ++ " at " ++ act ++ "."

-- |Error thrown when no parser for a constructor tag is found during parsing.
conLookupErr :: Monad m => forall a. String -> String -> m a
conLookupErr exp list = fail $ concat [ "Didn't find constructor tag ", exp, " in list ", list ]

-- |Error thrown when the constructor-parser-map is empty during parsing.
noCons :: Monad m => forall a. m a
noCons = fail "No constructor found during look-up."

-- |Error message for when an invalid version tag is read during parsing.
vNotFoundGetter :: String -> String
vNotFoundGetter s = "Cannot find getter associated with version " ++ s ++ "."

-- |Error message for when an invalid version tag is passed on during
-- serializing.
vNotFoundPutter :: String -> String
vNotFoundPutter s = "Cannot find putter associated with version " ++ s ++ "."

-- |Error message indicating that during back-compatible serialization no known
-- version of a datatype could be found.
idNotFoundPutter :: String -> [String] -> String
idNotFoundPutter s1 s2 =
    "Cannot find SmartCopy-Identifier " ++ s1 ++ " in identifier list " ++ show s2

-- |Error message for when a back-compatible format is used without a list of
-- known datatype IDs.
noIDListErr s
    = "Failed during back-compatible serialization of: " ++ s ++ ". Got no identifier list."

-------------------------------------------------------------------------------
-- Constructor and Field Types
-------------------------------------------------------------------------------

-- |Record containing constructor information needed for serialization and
-- deserialization of a datatype in formats of differing verbosity.
-- Not all fields are needed in all formats, e.g. binary
-- (de-)serialization doesn't require to know the constructor name or associated
-- fields.
data ConstrInfo
    = CInfo
    { cname :: T.Text
    , cfields :: Fields
    , ctagged :: Bool
    , cindex :: Integer
    , cidentifier :: String
    }
    deriving (Show, Eq)

-- |Information about the fields of a constructor.
-- NF (for Numbered Fields) indicates that a constructor has unlabeled fields
-- (with the Int parameter being the number of them).
-- LF (for Labeled Fields) indicates that a constructor has labeled fields
-- given by a list of labels.
-- This distinction should be sufficient for most formats, however Aeson
-- compatibility needs the additional alternative Empty, characterizing datatypes
-- where all constructors have 0 fields (as opposed to datatypes where at least
-- one constructor has more than 0 fields).
-- In the JSON format NF-0-field types are generally serialized as empty arrays,
-- whereas Empty-field-types are serialized as "String $ConName".
data Fields = NF Int
            | LF [Label]
            | Empty
    deriving (Show, Eq)

type Label = T.Text

-------------------------------------------------------------------------------
-- Version control functions
-------------------------------------------------------------------------------

-- |Serialize a versioned datatype by writing out its version tag as required
-- by the specific format.
-- When implementing a format that deals with versioned data, the serialization
-- function needs to call smartPut. For formats only dealing with
-- unversioned data, the serialization function calls the SmartCopy member function
-- writeSmart instead.
smartPut :: (SmartCopy a, Monad m) => SerializationFormat m -> a ->  m ()
smartPut fmt a
    = do putter <- getSmartPut fmt
         putter a

-- |Parse a versioned datatype by reading its version tag as required
-- by the specific format and returning a getter constructed from the version tag
-- to read the rest of the datatype.
-- When implementing a format that deals with versioned data, the parse
-- function needs to call smartGet. For formats only dealing with
-- unversioned data, the pasrse function calls the SmartCopy member function
-- readSmart instead.
smartGet :: (SmartCopy a, Monad m, Applicative m, Alternative m)
         => ParseFormat m
         -> m a
smartGet fmt = join $ getSmartGet fmt

-- |Analogous to the Data.SafeCopy function getSafePut:
-- Writes a version tag for a non-primitive datatype and returns a putter
-- for the rest of the datatype given by the concrete format implementation.
-- For primitive datatypes no version tag is written out and a simple putter
-- is returned that doesn't handle versions.
getSmartPut :: forall a m. (SmartCopy a, Monad m)
           => SerializationFormat m
           -> m (a -> m ())
getSmartPut fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ \a -> writeSmart fmt (asProxyType a proxy) Nothing
      _         ->
          do let ver = version :: Version a
             mkPutter fmt True (unVersion ver) Nothing
    where proxy = Proxy :: Proxy a

-- |Analogous to the Data.SafeCopy function getSafeGet:
-- Reads a version tag for a non-primitive datatype and returns a getter
-- for the rest of the datatype given by the concrete format implementation
-- and parsed version tag.
-- For primitive datatypes a simple getter for unversioned datatypes is returned
-- and no version tag is being read.
getSmartGet :: forall a m. (SmartCopy a, Monad m, Applicative m, Alternative m)
           => ParseFormat m
           -> m (m a)
getSmartGet fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ readSmart fmt
      kind -> mkGetter fmt True 0 Nothing
      where proxy = Proxy :: Proxy a

-- |Serialize a versioned datatype (and all inner datatypes) in the latest 
-- version that is known by a communicating component.
-- To account for the possibility that constructor names of evolved datatypes may 
-- change the identifier of a SmartCopy instance is looked up in a list of
-- known identifiers.
smartPutLastKnown :: forall a m. (SmartCopy a, Monad m)
                  => SerializationFormat m
                  -> a
                  -> [String]
                  -> m ()
smartPutLastKnown fmt a allIds =
    do putter <-
           getSmartPutLastKnown fmt allIds
       putter a

-- |Analogous to getSmartPut. Returns a putter for the latest version of a
-- datatype known by a communicating component.
-- When an identifier of an extension can't be found in the identifier list,
-- tests if its predecessor's identifier is present and possibly returns the
-- back-migrating putter.
getSmartPutLastKnown :: forall a m. (SmartCopy a, Monad m)
               => SerializationFormat m
               -> [String]
               -> m (a -> m ())
getSmartPutLastKnown fmt allIds =
       checkConsistency aProxy $
       case kindFromProxy aProxy of
         Primitive -> return $ \a -> writeSmart fmt a (Just allIds)
         Base ->
             if elem thisId allIds
                then mkPutter fmt True thisV (Just allIds)
                else fail (idNotFoundPutter thisId allIds)
         Extends bProxy ->
             if elem thisId allIds
                then mkPutter fmt True thisV (Just allIds)
                else liftM (. migrateBack) (getSmartPutLastKnown fmt allIds)
    where aProxy = Proxy :: Proxy a
          thisV = unVersion $ versionFromProxy (Proxy :: Proxy a)
          thisId = unId (identifier :: Identifier a)

-------------------------------------------------------------------------------
-- Migrate type class, version and kind types and helper functions
-------------------------------------------------------------------------------

-- |Corresponding to the Data.SafeCopy type class Migrate.
-- Type class specifies migrations from a previous version of a datatype to
-- a newer version.
class (SmartCopy (MigrateFrom a)) => Migrate a where
    -- |Type to be extended.
    type MigrateFrom a
    -- |Migrate function that specifies how to migrate from an older version of
    -- |a datatype to its newer one.
    migrateFwd :: MigrateFrom a -> a
    migrateBack :: a -> MigrateFrom a
        
-- | Simple Version ID.
newtype Version a = Version { unVersion :: Int32 } deriving (Eq, Show)

-- | A unique string representing a type at one particular version.
-- With this addition it can be made sure that when exchanging data
-- in a distributed system, everything is serialized in the latest version
--s till known by a component in a deprecated state.
newtype Identifier a = ID { unId :: String } deriving (Eq, Show)

instance S.Serialize (Version a) where
    get = liftM Version S.get
    put = S.put . unVersion

instance Num (Version a) where
    Version a + Version b = Version (a+b)
    Version a * Version b = Version (a*b)
    Version a - Version b = Version (a-b)
    negate (Version a) = Version (negate a)
    abs (Version a) = Version (abs a)
    signum (Version a) = Version (signum a)
    fromInteger i = Version (fromInteger i)

instance Ord (Version a) where
    Version a <= Version b = a <= b

instance Show (Kind a) where
    
castVersion :: Version a -> Version b
castVersion (Version a) = Version a

-- | Wrapper type used migrating backwards in the chain of compatible types.
newtype Reverse a = Reverse { unReverse :: a }

-- |A datatype's kind specifies how version control is handled.
-- For a type of kind Primitive, no version tag is written out
-- or read at all and there cannot be an extending type.
-- A type of kind Base is serialized writing out its version tag
-- and deserialized reading its version tag.
-- A type of kind Extension lets the system know that a previous
-- version of the datatype exists and can be migrated from (possibly
-- in multiple steps going back in the migration chain).
data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extends   :: Migrate a => Proxy (MigrateFrom a) -> Kind a
    Extended  :: (Migrate (Reverse a)) => Kind a -> Kind a

-- |The Extension kind lets the system know that a previous version
-- of the datatype exists. There can only be one direct predecessor
-- but it is possible to have chains of extensions.
extension :: (SmartCopy a, Migrate a) => Kind a
extension = Extends Proxy

-- |The extendedExtension kind lets the system know that there are
-- at least one future and previous version of this type.
extendedExtension :: (SmartCopy a, Migrate a, Migrate (Reverse a)) => Kind a
extendedExtension = Extended extension

-- |The extendedBase kind lets the system know that there is
-- at least one future version of this type.
extendedBase :: (Migrate (Reverse a)) => Kind a
extendedBase = Extended base

-- |The default type that is not an extension to any other type.
base :: Kind a
base = Base

-- |A type that cannot be extended and is not tagged with a version.
-- Unversioned (de-)serialization is not used for any build-in datatypes
-- but can be achieved by using the Prim wrapper or explicitly declaring
-- an instance's kind to be Primitive.
primitive :: Kind a
primitive = Primitive

-- |Construct a getter for a versioned type from a given version and kind.
-- This function is not intended for the standard user, but is called from the
-- mkGetter function of the concrete formats.
constructGetterFromVersion :: forall a m. (SmartCopy a, Monad m, Applicative m, Alternative m)
                           => ParseFormat m
                           -> Version a
                           -> Kind a
                           -> Either String (m a)
constructGetterFromVersion fmt diskV origK =
    worker fmt False diskV origK
    where
    worker :: forall a m. (SmartCopy a, Monad m, Applicative m, Alternative m)
           => ParseFormat m
           -> Bool
           -> Version a
           -> Kind a
           -> Either String (m a)
    worker fmt fwd thisV thisK
        | version == thisV = return $ readSmart fmt
        | otherwise =
          case thisK of
            Primitive -> Left "Cannot migrate from primitive types."
            Base -> Left $ vNotFoundGetter (show thisV)
            Extends bProxy ->
                do previousGetter <- worker fmt fwd (castVersion diskV) (kindFromProxy bProxy)
                   return $ fmap migrateFwd previousGetter
            Extended{} | fwd -> Left $ vNotFoundGetter (show thisV)
            Extended aKind ->
                do let revProxy :: Proxy (MigrateFrom (Reverse a))
                       revProxy = Proxy
                       forwardGetter :: Either String (m a)
                       forwardGetter
                           = fmap (unReverse . migrateFwd) <$>
                             worker fmt True (castVersion thisV) (kindFromProxy revProxy)
                       previousGetter :: Either String (m a)
                       previousGetter = worker fmt fwd (castVersion thisV) aKind
                   case forwardGetter of
                     Left{} -> previousGetter
                     Right val -> Right val

-------------------------------------------------------------------------------
-- Proxy type and helper functions
-------------------------------------------------------------------------------
data Proxy a = Proxy deriving (Show, Eq)

versionFromKind :: SmartCopy a => Kind a -> Version a
versionFromKind _ = version

versionFromProxy :: SmartCopy a => Proxy a -> Version a
versionFromProxy _ = version

kindFromProxy :: SmartCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

identFromProxy :: SmartCopy a => Proxy a -> Identifier a
identFromProxy _ = identifier

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a

-------------------------------------------------------------------------------
-- Consistency checking (from Data.SafeCopy)
-------------------------------------------------------------------------------
data Consistency a = Consistent | NotConsistent String

-- |Verify that the SmartCopy instance is consistent (same as the original
-- Data.SafeCopy funcion).
checkConsistency :: (SmartCopy a, Monad m) => Proxy a -> m b -> m b
checkConsistency proxy ks =
    case consistentFromProxy proxy of
      NotConsistent msg -> fail msg
      Consistent        -> ks

consistentFromProxy :: SmartCopy a => Proxy a -> Consistency a
consistentFromProxy _ = internalConsistency

internalConsistency :: SmartCopy a => Consistency a
internalConsistency = computeConsistency Proxy

{-# INLINE computeConsistency #-}
computeConsistency :: SmartCopy a => Proxy a -> Consistency a
computeConsistency proxy
    | isObviouslyConsistent (kindFromProxy proxy)
    = Consistent
    | versions /= nub versions
    = NotConsistent $ "Duplicate version tags: " ++ show versions
    | not (validChain proxy)
    = NotConsistent "Primitive types can not be extended as they have no version tag."
    | otherwise
    = Consistent
    where versions = availableVersions proxy

isObviouslyConsistent :: Kind a -> Bool
isObviouslyConsistent Primitive = True
isObviouslyConsistent Base = True
isObviouslyConsistent _ = False

validChain :: SmartCopy a => Proxy a -> Bool
validChain aProxy =
    worker (kindFromProxy aProxy)
    where worker Primitive = True
          worker Base = True
          worker (Extends bProxy) = check (kindFromProxy bProxy)
          check :: SmartCopy b => Kind b -> Bool
          check bKind =
              case bKind of
                Primitive -> False
                Base -> True
                Extends cProxy -> check (kindFromProxy cProxy)

availableVersions :: SmartCopy a => Proxy a -> [Int32]
availableVersions aProxy =
    worker True (kindFromProxy aProxy)
    where worker :: SmartCopy b => Bool -> Kind b -> [Int32]
          worker fwd bKind =
              case bKind of
                Primitive -> []
                Base -> [unVersion (versionFromKind bKind)]
                Extends bProxy ->
                    unVersion (versionFromKind bKind) : worker False (kindFromProxy bProxy)

