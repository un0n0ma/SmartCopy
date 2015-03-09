{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartCopy.Generic where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import qualified Data.List as L
import qualified Data.Text as T

import Text.Parsec (try)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import qualified Data.Proxy as P

import Control.Applicative
import Control.Monad (liftM)
import Data.Data hiding (Proxy)
import Data.Maybe
import GHC.Generics
import Generics.Deriving.ConNames

-------------------------------------------------------------------------------
-- Generic functions for versioned serializing/parsing
-------------------------------------------------------------------------------
gsmartPut :: (GSmartCopy f, Monad m, SmartCopy x)
          => SerializationFormat m
          -> f x
          -> m ()
gsmartPut fmt x =
    do putter <- ggetSmartPut fmt
       putter x

ggetSmartPut :: forall f x m. (GSmartCopy f, Monad m)
             => SerializationFormat m
             -> m (f x -> m ())
ggetSmartPut fmt =
--    checkConsistency proxy $
    case gkindFromProxy proxy of
      Primitive -> return $ \a -> gwriteSmart fmt a False 0 False Empty
      _         -> --let ver = version :: Version (f x) ---- TODO
                      return $ \a ->
                          gwriteSmart fmt (P.asProxyTypeOf a proxy) False 0 True Empty
                          -- Fix!
      where proxy = P.Proxy :: P.Proxy (f x)


-------------------------------------------------------------------------------
-- Rep instances
-------------------------------------------------------------------------------
instance GSmartCopy U1 where
    gwriteSmart fmt _ _ _ False _ = return ()
    gwriteSmart fmt _ _ _ True _ = return ()
    greadSmart fmt _ _ = return $ Right U1

instance (Datatype d, Selectors f, GSmartCopy f) => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _ versioned _
        = gwriteSmart fmt x False 0 versioned Empty
    greadSmart fmt _ ver
        = liftM (either Left (Right . M1)) $ greadSmart fmt [] ver

instance (Constructor c, Selectors f, GSmartCopy f) => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) multCons index ver fields
        = do fields' <-
                 if multCons
                    then return fields
                    else getFields 0 $ selectors (P.Proxy :: P.Proxy (M1 C c f)) 0
             let index'
                   | multCons
                   = index
                   | otherwise
                   = 0
             let cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields'
                          multCons index' True
             withCons fmt cons $ gwriteSmart fmt x multCons index' ver fields'
    greadSmart fmt [] ver
        = do fields <- getFields 0 $ selectors (P.Proxy :: P.Proxy (M1 C c f)) 0
             let cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields False 0 True
             readCons fmt [(cons, liftM (either Left (Right . M1)) $ greadSmart fmt [] ver)]
    greadSmart fmt conList ver
        = readCons fmt $ zip conList $ repeat $
          liftM (either Left (Right . M1)) $ greadSmart fmt [] ver

instance (Selector s, GSmartCopy f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) multCons conInd vers fields
        = withField fmt $ gwriteSmart fmt a multCons conInd vers fields
    greadSmart fmt _ ver
        = readField fmt $ liftM (either Left (Right . M1)) $ greadSmart fmt [] ver

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) _ _ _ _ = writeSmart fmt a
    greadSmart fmt _ _ = liftM (either Left (Right . K1)) $ readSmart fmt

instance (ConNames a, ConNames b, Selectors a, Selectors b, GSmartCopy a, GSmartCopy b)
         => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) _ conInd versioned fields
        = do let sels = selectors (P.Proxy :: P.Proxy (a :+: b)) conInd
             fields1 <- getFields conInd sels
             fields2 <- getFields (conInd + 1) sels
             let fields'
                   | fields1 == Empty && fields2 == Empty
                   = Empty
                   | fields1 == Empty && fields2 /= Empty
                   = NF 0
                   | otherwise
                   = fields1
             gwriteSmart fmt x True conInd versioned fields'
    gwriteSmart fmt (R1 x) _ conInd versioned fields
        = do let sels = selectors (P.Proxy :: P.Proxy (a :+: b)) conInd
             fields1 <- getFields conInd sels
             fields2 <- getFields (conInd + 1) sels
             let fields'
                   | fields1 == Empty && fields2 == Empty
                   = Empty
                   | fields1 /= Empty && fields2 == Empty
                   = NF 0
                   | otherwise
                   = fields2
             gwriteSmart fmt x True (conInd + 1) versioned fields'
    greadSmart fmt conList ver
        = do let cindex1
                   | null conList
                   = 0
                   | otherwise
                   = cindex (last conList) + 2
                 cindex2
                   | null conList
                   = 1
                   | otherwise
                   = cindex (last conList) + 2
                 sels = selectors (P.Proxy :: P.Proxy (a :+: b)) 0
             fields1 <- getFields cindex1 sels
             fields2 <- getFields cindex2 sels
             let fields1'
                   | fields1 == Empty && fields2 == Empty
                   = Empty
                   | fields1 == Empty && fields2 /= Empty
                   = NF 0
                   | otherwise
                   = fields1
                 fields2'
                   | fields1 == Empty && fields2 == Empty
                   = Empty
                   | fields1 /= Empty && fields2 == Empty
                   = NF 0
                   | otherwise
                   = fields2
                 cname1 = T.pack $ gconNameOf (undefined :: a f)
                 cname2 = T.pack $ gconNameOf (undefined :: b f)
                 -- Hack! Add dummy type to conlist to represent "isSumType".
                 -- "Tagged" bool is not sufficient, JSON needs to know
                 -- con-map length. Maybe there's a better way.
                 cList1 = [C cname1 fields1' True cindex1 True, emptyCons]
                 cList2 = [C cname2 fields2' True cindex2 True, emptyCons]
                 cList = cList1 ++ cList2
             withLookahead fmt cindex1
               (liftM (either Left (Right . L1)) $ greadSmart fmt cList1 ver)
               (liftM (either Left (Right . R1)) $ greadSmart fmt cList2 ver)

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) multCons conInd vers fields
        = do gwriteSmart fmt a multCons conInd vers fields
             gwriteSmart fmt b multCons conInd vers fields
    greadSmart fmt conList ver
        = do res1 :: Either String (a x) <- greadSmart fmt [] ver
             case res1 of
               Left msg -> return $ Left msg
               Right r ->
                   liftM (either Left (Right . (:*:) r)) $ greadSmart fmt [] ver

-------------------------------------------------------------------------------
-- Helper functions for Reps
-------------------------------------------------------------------------------
gkindFromProxy :: (GSmartCopy f) => P.Proxy (f x) -> Kind (f x)
gkindFromProxy _ = gkind

-------------------------------------------------------------------------------
-- Helper functions/types for accessing selector and constructor information
-------------------------------------------------------------------------------

class Selectors (rep :: * -> *) where
    selectors :: P.Proxy rep -> Integer -> [(Integer, [String])]

instance (Selectors f, Constructor c) => Selectors (M1 C c f) where
    selectors proxy conInd = selectors (P.Proxy :: P.Proxy f) conInd

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
    selectors proxy conInd =
        selectors (P.Proxy :: P.Proxy a) conInd ++
        selectors (P.Proxy :: P.Proxy b) (conInd + 1)

instance Selector s => Selectors (M1 S s (K1 R t)) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s (K1 R t) ())])]

instance Selector s => Selectors (M1 S s f) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s f x)])]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
    selectors _ conInd =
        let sels = map snd (selectors (P.Proxy :: P.Proxy a) conInd) ++
                   map snd (selectors (P.Proxy :: P.Proxy b) conInd)
        in [(conInd, concat sels)]

instance Selectors U1 where
    selectors _ conInd = [(conInd, [])]

getFields :: Monad m => Integer -> [(Integer, [String])] -> m Fields
getFields _ [] = return Empty
getFields conInd sels
    = let conFields = lookup conInd sels in
      case conFields of
        Just [] ->
            return Empty
        Just (x:xs) ->
            case x of
              "" -> return $ NF (length (x:xs))
              string -> return $ LF $ map T.pack (x:xs)
        Nothing ->
            fail $ "Didn't find fields for constructor index " ++
                   (show conInd) ++ " in " ++ (show sels)
    
getConNames :: (Generic a, ConNames (Rep a)) => a -> [T.Text]
getConNames = map T.pack . conNames
