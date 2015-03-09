{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Tests.TestInstancesMigrate where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.MonadTypesInstances
import SmartCopy.SmartCopy
import Tests.TestInstances as V1

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.SafeCopy as SC
import qualified Data.Serialize as B

import GHC.Generics

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------

--- V2 with added Maybe field
data EasyV2 = EasyV2 Int (Maybe String) deriving (Eq, Show, Generic)

easy :: EasyV2
easy = EasyV2 42 (Just "String")

versionSM = version :: Version EasyV2
version1SM = version :: Version V1.Easy

versionSC = SC.version :: SC.Version EasyV2
version1SC = SC.version :: SC.Version V1.Easy

--- V2 with deleted field
data SomeV2 = SomeV2 Spam deriving (Eq, Show, Generic)
data SomeV1 = SomeV1 Spam Int deriving (Eq, Show, Generic)

some :: SomeV2
some = SomeV2 (Spam 1)

someOld :: SomeV1
someOld = SomeV1 (Spam 1) 2

instance B.Serialize EasyV2
SC.deriveSafeCopy 2 'SC.extension ''EasyV2
SC.deriveSafeCopy 2 'SC.extension ''SomeV2
SC.deriveSafeCopy 1 'SC.base ''SomeV1

instance SC.Migrate EasyV2 where
    type MigrateFrom EasyV2 = V1.Easy
    migrate (V1.Easy i) = EasyV2 i Nothing

instance SC.Migrate SomeV2 where
    type MigrateFrom SomeV2 = SomeV1
    migrate (SomeV1 spam i) = SomeV2 spam

instance SmartCopy EasyV2 where
    version = 2
    kind = extension
    readSmart fmt =
        readCons fmt [(C "EasyV2" (NF 2) False 0 False, readEasy)]
        where
            readEasy =
                do get1 <- getSmartGet fmt
                   get2 <- getSmartGet fmt
                   f1 <- readField fmt get1 >>= either fail return
                   f2 <- readField fmt get2 >>= either fail return
                   return $ Right $ EasyV2 f1 f2
    writeSmart fmt (EasyV2 int string) =
        withCons fmt (C "EasyV2" (NF 2) False 0 False) writeFields
        where
            writeFields =
                do put1 <- getSmartPut fmt
                   put2 <- getSmartPut fmt
                   withField fmt $ put1 int
                   withField fmt $ put2 string

instance SmartCopy SomeV2 where
    version = 2
    kind = extension
    readSmart fmt =
        readCons fmt [(C "SomeV2" (NF 1) False 0 False, readSpam)]
        where
            readSpam =
                do getSpam <- getSmartGet fmt
                   sp <- readField fmt getSpam >>= either fail return
                   return $ Right $ SomeV2 sp
    writeSmart fmt (SomeV2 sp) =
        withCons fmt (C "SomeV2" (NF 1) False 0 False) $ withField fmt $ smartPut fmt sp

instance SmartCopy SomeV1 where
    version = 1
    kind = base
    writeSmart fmt (SomeV1 spam int) =
        withCons fmt (C "SomeV1" (NF 2) False 0 False) writeFields
        where
            writeFields =
                do putter1 <- getSmartPut fmt
                   putter2 <- getSmartPut fmt
                   withField fmt (putter1 spam)
                   withField fmt (putter2 int)
    readSmart fmt =
        readCons fmt [(C "SomeV1" (NF 2) False 0 False, readSome)]
        where readSome = do getSpam <- getSmartGet fmt
                            getInt <- getSmartGet fmt
                            spam <- readField fmt getSpam >>= either fail return
                            int <- readField fmt getInt >>= either fail return
                            return $ Right $ SomeV1 spam int


instance Migrate EasyV2 where
    type MigrateFrom EasyV2 = V1.Easy
    migrate (V1.Easy i) = EasyV2 i Nothing

instance Migrate SomeV2 where
    type MigrateFrom SomeV2 = SomeV1
    migrate (SomeV1 spam i) = SomeV2 spam
    
