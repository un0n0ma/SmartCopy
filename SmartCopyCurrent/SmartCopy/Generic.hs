{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

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
      Primitive -> return $ \a -> gwriteSmart fmt a False 0 (Version 0)
      _         -> do --let ver = version :: Version (f x) ---- TODO
                      --writeVersion fmt ver
                      return $ \a ->
                          withVersion fmt (Version 0) $
                          gwriteSmart fmt (P.asProxyTypeOf a proxy) False 0 (Version 0) -- Fix!
      where proxy = P.Proxy :: P.Proxy (f x)


-------------------------------------------------------------------------------
-- Rep instances
-------------------------------------------------------------------------------
instance GSmartCopy U1 where
    gwriteSmart fmt _ _ _ _ = return ()
    greadSmart fmt = return U1

instance (Datatype d, Selectors f, GSmartCopy f) => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _ ver
        = gwriteSmart fmt x False 0 (castVersion ver :: Version x)
    greadSmart fmt = M1 <$> greadSmart fmt

instance (Constructor c, Selectors f, GSmartCopy f) => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) multCons index ver
        = let fields = getFields index $ selectors (P.Proxy :: P.Proxy (M1 C c f)) index
              cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields
                       multCons index
          in withCons fmt cons $ gwriteSmart fmt x multCons index (castVersion ver :: Version x)
    greadSmart fmt = undefined

instance (Constructor c, Selector s, GSmartCopy f)
         => GSmartCopy (M1 C c (M1 S s f)) where
    gwriteSmart fmt con@(M1 x) multCons index _
        = let fields = getFields index $ selectors (P.Proxy :: P.Proxy (M1 C c (M1 S s f))) index
              cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields
                       multCons index
          in withCons fmt cons $ do putter <- ggetSmartPut fmt
                                    withField fmt $ putter x

instance (Selector s, GSmartCopy f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) _ _ _ = do putter <- ggetSmartPut fmt
                                      withField fmt $ putter a
    greadSmart fmt = readField fmt $ greadSmart fmt

instance SmartCopy c => GSmartCopy (K1 a c) where
--    gversion = castVersion (versionFromProxy (Proxy :: Proxy c)) -- TODO
    gwriteSmart fmt (K1 a) _ _ _ = writeSmart fmt a
    greadSmart fmt = liftM K1 (readSmart fmt)

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) _ conInd v
        = gwriteSmart fmt x True conInd (castVersion v :: Version x)
    gwriteSmart fmt (R1 x) _ conInd v
        = gwriteSmart fmt x True (conInd + 1) (castVersion v :: Version x)
    greadSmart fmt = L1 <$> greadSmart fmt
                        <|> R1 <$> greadSmart fmt

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) _ _ _
        = do putterA <- ggetSmartPut fmt
             putterB <- ggetSmartPut fmt
             withField fmt (putterA a)
             withField fmt (putterB b)
    greadSmart fmt = (:*:) <$> readField fmt (greadSmart fmt) <*> readField fmt (greadSmart fmt)

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

instance (Constructor c, Selectors b, Selectors f) => Selectors ((M1 C c f) :+: b) where
    selectors proxy index =
        selectors (P.Proxy :: P.Proxy f) index ++
        selectors (P.Proxy :: P.Proxy b) (index + 1)

instance Selector s => Selectors (M1 S s (K1 R t)) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s (K1 R t) ())])]

instance Selector s => Selectors (M1 S s f) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s f x)])]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
    selectors _ conInd = selectors (P.Proxy :: P.Proxy a) conInd ++ selectors (P.Proxy :: P.Proxy b) conInd

instance Selectors U1 where
    selectors _ _ = []

getFields :: Integer -> [(Integer, [String])] -> Fields
getFields _ [] = Empty
getFields conInd sels
    = let Just (x:xs) = lookup conInd sels in
      case x of
          "" -> NF (length (x:xs))
          string -> LF $ map T.pack (x:xs)
    
getConNames :: (Generic a, ConNames (Rep a)) => a -> [T.Text]
getConNames = map T.pack . conNames
