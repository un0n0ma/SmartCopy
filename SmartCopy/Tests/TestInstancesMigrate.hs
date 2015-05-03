{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.TestInstancesMigrate where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import qualified Tests.TestInstances as V1

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.SmartCopy

import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Data.Data
import Data.Typeable
import GHC.Generics

data Easy = Easy String (Maybe String) deriving (Eq, Show, Generic)

--- V2 with added Maybe field
data EasyV2 = EasyV2 Int (Maybe String) deriving (Eq, Show, Generic)

--- V3 with newer version of Spam
data Some = Some Spam deriving (Eq, Show, Generic)

--- V2 with deleted field
data SomeV2 = SomeV2 SpamV1 deriving (Eq, Show, Generic)

data SomeV1 = SomeV1 SpamV1 Int deriving (Eq, Show, Generic)

data Spam = Spam Int (Maybe Int) deriving (Eq, Show, Generic)

data SpamV1 = SpamV1 Int deriving (Eq, Show, Generic)

--- Migrate instances ---------------------------------------------------------

instance Migrate Spam where
    type MigrateFrom Spam = SpamV1
    migrateFwd (SpamV1 i) = Spam i Nothing
    migrateBack (Spam i _) = SpamV1 i

instance Migrate Easy where
    type MigrateFrom Easy = EasyV2
    migrateFwd (EasyV2 i mb) = Easy (show i) mb
    migrateBack (Easy str mb) = EasyV2 (readDef str) mb
        where readDef str =
                  case reads str of
                    [(x, "")] -> x
                    _ -> 0

instance Migrate EasyV2 where
    type MigrateFrom EasyV2 = V1.Easy
    migrateFwd (V1.Easy i) = EasyV2 i Nothing
    migrateBack (EasyV2 i _) = V1.Easy i

instance Migrate SomeV2 where
    type MigrateFrom SomeV2 = SomeV1
    migrateFwd (SomeV1 spam i) = SomeV2 spam
    migrateBack (SomeV2 spam) = SomeV1 spam 0

instance Migrate Some where
    type MigrateFrom Some = SomeV2
    migrateFwd (SomeV2 (SpamV1 i1)) = Some (Spam i1 Nothing)
    migrateBack (Some (Spam i1 _)) = SomeV2 (SpamV1 i1)

--- Other instances ---------------------------------------------------------
instance SmartCopy Some where
    identifier = ID "SomeV3"
    version = 3
    kind = extension

instance SmartCopy SpamV1 where
    identifier = ID "SpamV1"
    version = 1
    kind = base

instance SmartCopy Spam where
    identifier = ID "SpamV2"
    version = 2
    kind = extension

instance SmartCopy EasyV2 where
    identifier = ID "EasyV2"
    version = 2
    kind = extension
    readSmart fmt =
        readCons fmt
            [(CInfo "EasyV2" (NF 2) False 0 $ unId (identifier :: Identifier EasyV2), readEasy)]
        where
            readEasy =
                do get1 <- getSmartGet fmt
                   get2 <- getSmartGet fmt
                   f1 <- readField fmt get1
                   f2 <- readField fmt get2
                   return $ EasyV2 f1 f2
    writeSmart fmt x@(EasyV2 int string) mIds =
        do let writeFields =
                 case mIds of
                   Nothing -> 
                      do put1 <- getSmartPut fmt
                         put2 <- getSmartPut fmt
                         withField fmt $ put1 int
                         withField fmt $ put2 string
                   Just allIds ->
                      do put1 <- getSmartPutLastKnown fmt allIds
                         put2 <- getSmartPutLastKnown fmt allIds
                         withField fmt $ put1 int
                         withField fmt $ put2 string
           withCons fmt
               (CInfo "EasyV2" (NF 2) False 0 $ unId (identifier :: Identifier EasyV2)) writeFields

instance SmartCopy Easy
    where identifier = ID "EasyV3"
          version = 3
          kind = extension

instance SmartCopy SomeV2 where
    identifier = ID "SomeV2"
    version = 2
    kind = extension
    readSmart fmt =
        readCons fmt
            [(CInfo "SomeV2" (NF 1) False 0 $ unId (identifier :: Identifier SomeV2), readSpam)]
        where
            readSpam =
                do getSpam <- getSmartGet fmt
                   sp <- readField fmt getSpam
                   return $ SomeV2 sp
    writeSmart fmt x@(SomeV2 sp) mIds =
        do let writeField =
                 case mIds of
                   Nothing -> withField fmt $ smartPut fmt sp
                   Just allIds -> withField fmt $ smartPutLastKnown fmt sp allIds
           withCons fmt
               (CInfo "SomeV2" (NF 1) False 0 $ unId (identifier :: Identifier SomeV2)) writeField

instance SmartCopy SomeV1 where
    identifier = ID "SomeV1"
    version = 1
    kind = base
    writeSmart fmt x@(SomeV1 spam int) mIds =
        do let writeFields =
                 case mIds of
                   Nothing ->
                       do putter1 <- getSmartPut fmt
                          putter2 <- getSmartPut fmt
                          withField fmt (putter1 spam)
                          withField fmt (putter2 int)
                   Just allIds ->
                       do putter1 <- getSmartPutLastKnown fmt allIds
                          putter2 <- getSmartPutLastKnown fmt allIds
                          withField fmt (putter1 spam)
                          withField fmt (putter2 int)
           withCons fmt
               (CInfo "SomeV1" (NF 2) False 0 $ unId (identifier :: Identifier SomeV1)) writeFields
    readSmart fmt =
        readCons fmt
            [(CInfo "SomeV1" (NF 2) False 0 $ unId (identifier :: Identifier SomeV1), readSome)]
        where readSome = do getSpam <- getSmartGet fmt
                            getInt <- getSmartGet fmt
                            spam <- readField fmt getSpam
                            int <- readField fmt getInt
                            return $ SomeV1 spam int

instance B.Serialize EasyV2
instance B.Serialize Easy
instance B.Serialize SomeV2
instance B.Serialize Some
instance B.Serialize Spam
instance B.Serialize SpamV1

deriving instance Typeable Some
deriving instance Typeable SpamV1
deriving instance Typeable Spam

instance SC.SafeCopy EasyV2 where
    version = 2
    kind = SC.extension
    putCopy (EasyV2 i ms)
        = SC.contain $
              do putter1 <- SC.getSafePut
                 putter2 <- SC.getSafePut
                 putter1 i
                 putter2 ms
    getCopy = SC.contain $
                  do getter1 <- SC.getSafeGet
                     getter2 <- SC.getSafeGet
                     i <- getter1
                     ms <- getter2
                     return $ EasyV2 i ms

instance SC.SafeCopy SomeV1 where
    version = 1
    kind = SC.base
    putCopy (SomeV1 spam i)
        = SC.contain $
              do putter1 <- SC.getSafePut
                 putter2 <- SC.getSafePut
                 putter1 spam
                 putter2 i
    getCopy = SC.contain $
                  do getter1 <- SC.getSafeGet
                     getter2 <- SC.getSafeGet
                     i <- getter1
                     spam <- getter2
                     return $ SomeV1 i spam

SC.deriveSafeCopy 3 'SC.extension ''Easy
SC.deriveSafeCopy 2 'SC.extension ''SomeV2
SC.deriveSafeCopy 3 'SC.extension ''Some
SC.deriveSafeCopy 1 'SC.base ''SpamV1
SC.deriveSafeCopy 2 'SC.extension ''Spam

instance SC.Migrate EasyV2 where
    type MigrateFrom EasyV2 = V1.Easy
    migrate (V1.Easy i) = EasyV2 i Nothing

instance SC.Migrate Easy where
    type MigrateFrom Easy = EasyV2
    migrate (EasyV2 i m) = Easy (show i) m 

instance SC.Migrate SomeV2 where
    type MigrateFrom SomeV2 = SomeV1
    migrate (SomeV1 spam i) = SomeV2 spam

instance SC.Migrate Some where
    type MigrateFrom Some = SomeV2
    migrate (SomeV2 (SpamV1 i)) = Some (Spam i Nothing)

instance SC.Migrate Spam where
    type MigrateFrom Spam = SpamV1
    migrate (SpamV1 i) = Spam i Nothing
--- Values --------------------------------------------------------------------

easyOld :: V1.Easy 
easyOld = V1.Easy 42

easy :: EasyV2
easy = EasyV2 42 (Just "String")

easyNothing :: EasyV2
easyNothing = EasyV2 42 Nothing

easy' :: Easy
easy' = Easy "42" (Just "String")

easyNothing' :: Easy
easyNothing' = Easy "42" Nothing

some :: SomeV2
some = SomeV2 (SpamV1 1)

someOld :: SomeV1
someOld = SomeV1 (SpamV1 1) 0

someNewSpam :: Some
someNewSpam = Some (Spam 1 Nothing)

someNewSpam' :: Some
someNewSpam' = Some (Spam 1 (Just 42))
