{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PackageImports #-}
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
       , mismatch
       , noCons
       , conLookupErr
       , Cons (..)
       , Fields (..)
       , base
       , extension
       , Prim (..)
       , Version (..)
       )
where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.MonadTypesInstances

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as S

import Data.Int
import Data.List (nub)
import Data.Text.Internal as T

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (MonadTrans(..))
import "mtl" Control.Monad.Writer

import GHC.Generics

class SmartCopy a where
    version :: Version a
    version = Version 0
    kind :: Kind a
    kind = Base
    writeSmart :: Monad m => SerializationFormat m -> a -> m ()
    default writeSmart :: (Generic a, GSmartCopy (Rep a), Monad m)
                       => SerializationFormat m -> a -> m ()
    writeSmart fmt a = gwriteSmart fmt (from a)
    readSmart :: (Applicative m, Alternative m, Monad m) => ParseFormat m -> m a
    readSmart fmt = fmap to (greadSmart fmt)
    default readSmart :: (Generic a, GSmartCopy (Rep a), Monad m, Applicative m, Alternative m)
                      => ParseFormat m -> m a

class GSmartCopy t where
    gwriteSmart :: Monad m => SerializationFormat m -> t x -> m ()
    greadSmart :: (Functor m, Applicative m, Monad m, Alternative m) => ParseFormat m -> m (t x)

instance SmartCopy a => SmartCopy (SC.Prim a) where
    kind = primitive
    readSmart fmt =
        do res <- readSmart fmt
           return $ SC.Prim res
    writeSmart fmt (SC.Prim a) =
        writeSmart fmt a

instance SmartCopy Int where
    readSmart fmt =
        do prim <- readInt fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writeInt fmt $ PrimInt i

instance SmartCopy Int32 where
    readSmart fmt =
        do prim <- readInt fmt
           fromPrimInt prim
        where fromPrimInt prim =
                  case prim of
                    PrimInt i -> return $ fromIntegral i
                    f         -> mismatch "int prim" (show f)
    writeSmart fmt i =
        writeInt fmt $ PrimInt $ fromIntegral i

instance SmartCopy Char where
    readSmart fmt =
        do prim <- readChar fmt
           fromPrimChar prim
        where fromPrimChar prim =
                  case prim of
                    PrimChar c -> return c
                    f         -> mismatch "Char prim" (show f)
    writeSmart fmt c =
        writeChar fmt $ PrimChar c

instance SmartCopy Double where
    readSmart fmt = 
        do prim <- readDouble fmt
           case prim of
             PrimDouble d -> return d
             f         -> mismatch "Double prim" (show f)
    writeSmart fmt d = writeDouble fmt (PrimDouble d)


instance SmartCopy String where
    readSmart fmt =
        do prim <- readString fmt
           fromPrimString prim
        where fromPrimString prim =
                  case prim of
                    PrimString s -> return s
                    f         -> mismatch "String prim" (show f)
    writeSmart fmt s =
        writeString fmt $ PrimString s

instance SmartCopy Bool where
    readSmart fmt =
        do prim <- readBool fmt
           fromPrimBool prim
        where fromPrimBool prim =
                  case prim of
                    PrimBool b -> return b
                    f         -> mismatch "Bool prim" (show f)
    writeSmart fmt b =
        writeBool fmt $ PrimBool b

instance SmartCopy a => SmartCopy (Maybe a) where
    readSmart = readMaybe
    writeSmart = writeMaybe

instance SmartCopy a => SmartCopy [a] where
    readSmart = readRepetition
    writeSmart = writeRepetition

instance (SmartCopy a, SmartCopy b) => SmartCopy (a, b) where
    readSmart fmt = liftM2 (,) (readSmart fmt) (readSmart fmt)
    writeSmart fmt (a, b) = writeSmart fmt a >> writeSmart fmt b

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

-------------------------------------------------------------------------------
-- Format records
-------------------------------------------------------------------------------

data SerializationFormat m
    = SerializationFormat
    { withVersion :: forall a. Version a -> m () -> m ()
    , withCons :: Cons -> m () -> m ()
    , withField :: m () -> m ()
    , writeRepetition :: SmartCopy a => [a] -> m ()
    , writeInt :: Prim -> m ()
    , writeInteger :: Prim -> m ()
    , writeChar :: Prim -> m ()
    , writeBool :: Prim -> m ()
    , writeDouble :: Prim -> m ()
    , writeString :: Prim -> m ()
    , writeVersion :: forall a. Version a -> m ()
    , writeMaybe :: SmartCopy a => Maybe a -> m ()
    }

data ParseFormat m
    = ParseFormat
    { readVersioned :: forall a. m a -> m a
    , readVersion :: forall a. m (Maybe (Version a))
    , readCons :: forall a. [(Cons, m a)] -> m a
    , readField :: forall a. m a -> m a
    , readRepetition :: SmartCopy a => m [a]
    , readInt :: m Prim
    , readChar :: m Prim
    , readBool :: m Prim
    , readDouble :: m Prim
    , readString :: m Prim
    , readMaybe :: SmartCopy a => m (Maybe a)
    }


-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

mismatch :: Monad m => forall a. String -> String -> m a
mismatch exp act = fail $ "Was expecting " ++ exp ++ " at " ++ act ++ "."

conLookupErr :: Monad m => forall a. String -> String -> m a
conLookupErr exp list = fail $ concat [ "Didn't find constructor tag "
                                      , exp, " in list ", list ]

noCons :: Monad m => forall a. m a
noCons = fail "No constructor found during look-up."

vNotFound :: String -> String
vNotFound s = "Cannot find getter associated with version " ++ s ++ "."
-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Cons
    = C
    { cname :: T.Text
    , cfields :: Fields
    , ctagged :: Bool
    , cindex :: Integer
    }

data Fields = NF Int
            | LF [Label]
            | Empty --- Empty is for types where no constructor has fields (differently represented in JSON than Cons .. 0 .. ..)

type Label = T.Text

data Prim = PrimInt Int
          | PrimInteger Integer
          | PrimChar Char
          | PrimString String
          | PrimBool Bool
          | PrimDouble Double
          deriving (Show, Read)


-------------------------------------------------------------------------------
-- Version control
-------------------------------------------------------------------------------

-- When implementing a format that deals with versioned data,
-- those are the functions to be used in serializeSmart and parseSmart.
-- In unversioned formats the functions can directly run readSmart and writeSmart.

smartPut :: (SmartCopy a, Monad m) => SerializationFormat m -> a ->  m ()
smartPut fmt a =
    do putter <- getSmartPut fmt
       putter a

smartGet :: (SmartCopy a, Monad m, Applicative m, Alternative m)
         => ParseFormat m
         -> m a
smartGet fmt = join $ getSmartGet fmt

-- Same as SmartCopy but dealing with arbitrary monads.

getSmartPut :: forall a m. (SmartCopy a, Monad m)
           => SerializationFormat m
           -> m (a -> m ())
getSmartPut fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ \a -> writeSmart fmt $ asProxyType a proxy
      _         -> do let ver = versionFromProxy proxy
                      writeVersion fmt ver
                      return $ \a -> withVersion fmt ver $ writeSmart fmt $ asProxyType a proxy
    where proxy = Proxy :: Proxy a

getSmartGet :: forall a m. (SmartCopy a, Monad m, Applicative m, Alternative m)
           => ParseFormat m
           -> m (m a)
getSmartGet fmt =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ readSmart fmt
      kind -> do v <- readVersion fmt :: m (Maybe (Version a))
                 case v of
                   Just  v' ->
                       case constructGetterFromVersion fmt v' kind of
                         Right getter -> return getter
                         Left msg -> fail msg
                   Nothing ->
                       return $ readSmart fmt
      where proxy = Proxy :: Proxy a


-- Migrate class

class SmartCopy (MigrateFrom a) => Migrate a where
    type MigrateFrom a
    migrate :: MigrateFrom a -> a

-- Types and utility functions from SafeCopy (SafeCopy exports are not sufficient)

newtype Version a = Version { unVersion :: Int32 } deriving (Eq, Show)

castVersion :: Version a -> Version b
castVersion (Version a) = Version a

newtype Reverse a = Reverse { unReverse :: a }

data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extends   :: Migrate a => Proxy (MigrateFrom a) -> Kind a
    Extended  :: (Migrate (Reverse a)) => Kind a -> Kind a

extension :: (SmartCopy a, Migrate a) => Kind a
extension = Extends Proxy

base :: Kind a
base = Base

primitive :: Kind a
primitive = Primitive

data Proxy a = Proxy

versionFromProxy :: SmartCopy a => Proxy a -> Version a
versionFromProxy _ = version

versionFromKind :: SmartCopy a => Kind a -> Version a
versionFromKind _ = version

kindFromProxy :: SmartCopy a => Proxy a -> Kind a
kindFromProxy _ = kind

asProxyType :: a -> Proxy a -> a
asProxyType a _ = a

mkProxy :: a -> Proxy a
mkProxy _ = Proxy

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
        | version == thisV = return $ readVersioned fmt $ readSmart fmt
        | otherwise =
          case thisK of
            Primitive -> Left "Cannot migrate from primitive types."
            Base -> Left $ vNotFound (show thisV)
            Extends bProxy ->
                do previousGetter <- worker fmt fwd (castVersion diskV) (kindFromProxy bProxy)
                   return $ fmap migrate previousGetter
            Extended{} | fwd -> Left $ vNotFound (show thisV)
            Extended aKind ->
                do let revProxy :: Proxy (MigrateFrom (Reverse a))
                       revProxy = Proxy
                       forwardGetter :: Either String (m a)
                       forwardGetter = fmap (unReverse . migrate) <$>
                                       worker fmt True (castVersion thisV) (kindFromProxy revProxy)
                       previousGetter :: Either String (m a)
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
