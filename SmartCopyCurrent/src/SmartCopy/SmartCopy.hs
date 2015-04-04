{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module SmartCopy.SmartCopy
       ( SmartCopy (..)
       , GSmartCopy (..)
       , Migrate (..)
       , SerializationFormat (..)
       , ParseFormat (..)
       , getSmartPut
       , getSmartGet
       , smartPut
       , smartGet
       , smartPutWithVersion
       , constructGetterFromVersion
       , mismatch
       , mismatchFail
       , noCons
       , conLookupErr
       , ConstrInfo (..)
       , Fields (..)
       , emptyCons
       , base
       , extension
       , primitive
       , Version (..)
       , Kind (..)
       , Reverse
       , Proxy (..)
       , asProxyType
       , mkProxy
       , versionFromProxy
       , kindFromProxy
       , castVersion
       , castKind
       , versionMap
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.MonadTypesInstances

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S

import Data.Int (Int32)
import Data.List (nub)
import Data.Text.Internal as T
import Text.Parsec hiding (Empty)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (MonadTrans(..))
import "mtl" Control.Monad.Writer

import Control.Applicative
import Control.Monad.IO.Class
import Data.Typeable hiding (Proxy)
import GHC.Generics

class SmartCopy a where
    version :: Version a
    version = 0
    kind :: Kind a
    kind = Base
    writeSmart :: Monad m => SerializationFormat m -> a -> m ()
    default writeSmart :: (Generic a, GSmartCopy (Rep a), Monad m)
                       => SerializationFormat m -> a -> m ()
    writeSmart fmt a
        = do wrapped <- gwriteSmart fmt (from a) [] []
             putter <- wrapped
             putter (from a)
    readSmart :: (Applicative m, Alternative m, Monad m) => ParseFormat m -> m (Either String a)
    default readSmart :: (Generic a, GSmartCopy (Rep a), Monad m, Applicative m, Alternative m)
                      => ParseFormat m -> m (Either String a)
    readSmart fmt
        = do wrapped <- greadSmart fmt [] []
             getter <- wrapped
             fmap (fmap to) getter

class GSmartCopy t where
    gwriteSmart :: Monad m
                => SerializationFormat m
                -> t x
                -> [ConstrInfo]
                -> [(Maybe TypeRep, Int32)]
                -> m (m (t x -> m ()))
    greadSmart :: (Functor m, Applicative m, Monad m, Alternative m)
               => ParseFormat m
               -> [ConstrInfo]
               -> [(Maybe TypeRep, Int32)]
               -> m (m (m (Either String (t x))))
-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

data SerializationFormat m
    = SerializationFormat
    { mkPutter :: SmartCopy a => Bool -> Int32 -> m (a -> m ())
    --- Bool: save byte by not writing out version
    , withCons :: ConstrInfo -> m () -> m ()
    , withField :: m () -> m ()
    , writeRepetition :: SmartCopy a => [a] -> m ()
    , writeInt :: Int -> m ()
    , writeInteger :: Integer -> m ()
    , writeChar :: Char -> m ()
    , writeBool :: Bool -> m ()
    , writeDouble :: Double -> m ()
    , writeString :: String -> m ()
    , writeMaybe :: SmartCopy a => Maybe a -> m ()
    , writeBS :: BS.ByteString -> m ()
    , writeText :: T.Text -> m ()
    }

data ParseFormat m
    = ParseFormat
    { mkGetter :: SmartCopy a => Bool -> Int32 -> m (m (Either String a))
    --- Bool: save byte by not writing out version. Int32: Version of duplicate type.
    , readCons :: forall a. [(ConstrInfo, m (Either String a))] -> m (Either String a)
    , readField :: forall a. m (Either String a) -> m (Either String a)
    , readRepetition :: SmartCopy a => m (Either String [a])
    , readInt :: m (Either String Int)
    , readChar :: m (Either String Char)
    , readBool :: m (Either String Bool)
    , readDouble :: m (Either String Double)
    , readString :: m (Either String String)
    , readMaybe :: SmartCopy a => m (Either String (Maybe a))
    , readBS :: m (Either String BS.ByteString)
    , readText :: m (Either String T.Text)
    }


-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

mismatch :: Monad m => forall a. String -> String -> m (Either String a)
mismatch exp act = return $ Left $ "Was expecting " ++ exp ++ " at " ++ act ++ "."

mismatchFail :: Monad m => String -> String -> m ()
mismatchFail exp act = fail $ "Was expecting " ++ exp ++ " at " ++ act ++ "."


conLookupErr :: Monad m => forall a. String -> String -> m (Either String a)
conLookupErr exp list = return $ Left $ concat [ "Didn't find constructor tag "
                                               , exp, " in list ", list ]
noCons :: Monad m => forall a. m (Either String a)
noCons = return $ Left "No constructor found during look-up."

vNotFound :: String -> String
vNotFound s = "Cannot find getter associated with version " ++ s ++ "."

vNotFoundPutter :: String -> String
vNotFoundPutter s = "Cannot find putter associated with version " ++ s ++ "."
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data ConstrInfo
    = CInfo
    { cname :: T.Text
    , cfields :: Fields
    , ctagged :: Bool
    , cindex :: Integer
    }
    deriving (Show, Eq)


data Fields = NF Int
            | LF [Label]
            | Empty
    deriving (Show, Eq)
            --- Empty is for types where no constructor has fields (differently represented in JSON than Cons .. 0 .. ..)

type Label = T.Text

emptyCons :: ConstrInfo
emptyCons = CInfo T.empty Empty False 0

-------------------------------------------------------------------------------
-- Version control
-------------------------------------------------------------------------------

-- When implementing a format that deals with versioned data,
-- those are the functions to be used in serializeSmart and parseSmart.
-- In unversioned formats the functions can directly run readSmart and writeSmart.

smartPut :: (SmartCopy a, Monad m) => SerializationFormat m -> a ->  m ()
smartPut fmt a
    = do putter <- getSmartPut fmt
         putter a

smartGet :: (SmartCopy a, Monad m, Applicative m, Alternative m)
         => ParseFormat m
         -> m (Either String a)
smartGet fmt = join $ getSmartGet fmt

-- Same as SmartCopy but dealing with arbitrary monads.

getSmartPut :: forall a m. (SmartCopy a, Monad m)
           => SerializationFormat m
           -> m (a -> m ())
getSmartPut fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ \a -> writeSmart fmt $ asProxyType a proxy
      _         -> do let ver = version :: Version a
                      mkPutter fmt True (unVersion ver)
    where proxy = Proxy :: Proxy a

getSmartGet :: forall a m i. (SmartCopy a, Monad m, Applicative m, Alternative m)
           => ParseFormat m
           -> m (m (Either String a))
getSmartGet fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ readSmart fmt
      kind -> mkGetter fmt True 0
      where proxy = Proxy :: Proxy a

-- Serialize in a particular version of the datatype

smartPutWithVersion :: (SmartCopy a, Monad m)
                    => SerializationFormat m
                    -> a
                    -> Int32
                    -> m ()
smartPutWithVersion fmt a version =
    do putter <- getSmartPutWithVersion fmt (versionMap a) version
       putter a

getSmartPutWithVersion :: forall a m. (SmartCopy a, Monad m)
               => SerializationFormat m
               -> VersionMap a
               -> Int32
               -> m (a -> m ())
getSmartPutWithVersion fmt vMap version =
    checkConsistency proxy $
        do let VersionMap verMap = vMap
           case M.lookup (Version version) verMap of
             Just typeId -> mkPutter fmt True $ unVersion (Version version :: Version typeId)
             Nothing -> fail $ vNotFoundPutter (show version)
    where proxy = Proxy :: Proxy a

-- Migrate

class SmartCopy (MigrateFrom a) => Migrate a where
    type MigrateFrom a
    migrate :: MigrateFrom a -> a

data VersionMap a = VersionMap (M.Map (Version a) (Proxy a)) deriving (Show, Eq)

versionMap :: forall a. SmartCopy a => a -> VersionMap a
versionMap a
    = case kindFromProxy aProxy of
        Primitive -> VersionMap $ M.singleton (Version 0) aProxy
        Base -> VersionMap $ M.singleton (versionFromProxy aProxy) aProxy
        Extends bProxy ->
            let VersionMap bMap = versionMap (undefined :: bProxy)
            in VersionMap $ M.insert (versionFromProxy aProxy) aProxy bMap
        Extended aKind ->
            let revProxy :: Proxy (MigrateFrom (Reverse a))
                revProxy = Proxy
            in versionMap (undefined :: revProxy)
      where aProxy = Proxy :: Proxy a

-- Types and utility functions from SafeCopy (SafeCopy exports are not sufficient)

newtype Version a = Version { unVersion :: Int32 } deriving (Eq, Show)

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

castVersion :: Version a -> Version b
castVersion (Version a) = Version a

castKind :: Kind a -> Kind b
castKind Primitive = Primitive
castKind Base = Base
--- TODO: Add extends, extended

newtype Reverse a = Reverse { unReverse :: a }

data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extends   :: Migrate a => Proxy (MigrateFrom a) -> Kind a
    Extended  :: (Migrate (Reverse a)) => Kind a -> Kind a

extension :: (SmartCopy a, Migrate a) => Kind a
extension = Extends Proxy

extendedExtension :: (SmartCopy a, Migrate a, Migrate (Reverse a)) => Kind a
extendedExtension = Extended extension

extendedBase :: (Migrate (Reverse a)) => Kind a
extendedBase = Extended base

base :: Kind a
base = Base

primitive :: Kind a
primitive = Primitive

data Proxy a = Proxy deriving (Show, Eq)

versionFromKind :: SmartCopy a => Kind a -> Version a
versionFromKind _ = version

versionFromProxy :: SmartCopy a => Proxy a -> Version a
versionFromProxy _ = version

kindFromProxy :: SmartCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

constructGetterFromVersion :: forall a m i. (SmartCopy a, Monad m, Applicative m, Alternative m)
                           => ParseFormat m
                           -> Version a
                           -> Kind a
                           -> Either String (m (Either String a))
constructGetterFromVersion fmt diskV origK =
    worker fmt False diskV origK
    where
    worker :: forall a m. (SmartCopy a, Monad m, Applicative m, Alternative m)
           => ParseFormat m
           -> Bool
           -> Version a
           -> Kind a
           -> Either String (m (Either String a))
    worker fmt fwd thisV thisK
        | version == thisV = return $ readSmart fmt
        | otherwise =
          case thisK of
            Primitive -> Left "Cannot migrate from primitive types."
            Base -> Left $ vNotFound (show thisV)
            Extends bProxy ->
                do previousGetter <- worker fmt fwd (castVersion diskV) (kindFromProxy bProxy)
                   return $ fmap (fmap migrate) previousGetter
            Extended{} | fwd -> Left $ vNotFound (show thisV)
            Extended aKind ->
                do let revProxy :: Proxy (MigrateFrom (Reverse a))
                       revProxy = Proxy
                       forwardGetter :: Either String (m (Either String a))
                       forwardGetter
                           = fmap (fmap (unReverse . migrate)) <$>
                             worker fmt True (castVersion thisV) (kindFromProxy revProxy)
                       previousGetter :: Either String (m (Either String a))
                       previousGetter = worker fmt fwd (castVersion thisV) aKind
                   case forwardGetter of
                     Left{} -> previousGetter
                     Right val -> Right val


-- Consistency (from SafeCopy)

data Consistency a = Consistent | NotConsistent String

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
validChain a_proxy =
    worker (kindFromProxy a_proxy)
    where worker Primitive = True
          worker Base = True
          worker (Extends b_proxy) = check (kindFromProxy b_proxy)
          check :: SmartCopy b => Kind b -> Bool
          check b_kind =
              case b_kind of
                Primitive -> False
                Base -> True
                Extends c_proxy -> check (kindFromProxy c_proxy)

availableVersions :: SmartCopy a => Proxy a -> [Int32]
availableVersions a_proxy =
    worker True (kindFromProxy a_proxy)
    where worker :: SmartCopy b => Bool -> Kind b -> [Int32]
          worker fwd b_kind =
              case b_kind of
                Primitive -> []
                Base -> [unVersion (versionFromKind b_kind)]
                Extends b_proxy ->
                    unVersion (versionFromKind b_kind) : worker False (kindFromProxy b_proxy)

