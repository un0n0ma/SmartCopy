{-# LANGUAGE AllowAmbiguousTypes #-}
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
      Primitive -> return $ \a -> gwriteSmart fmt a
      _         -> do let ver = castVersion (gversion :: Version (f x)) :: Version x
                      writeVersion fmt ver
                      return $ \a -> withVersion fmt ver $ gwriteSmart fmt $ P.asProxyTypeOf a proxy
      where proxy = P.Proxy :: P.Proxy (f x)


-------------------------------------------------------------------------------
-- Rep instances
-------------------------------------------------------------------------------

instance GSmartCopy U1 where
    gwriteSmart fmt _ = return ()
    greadSmart fmt = return U1

instance (Datatype d, Selectors f, GSmartCopy f) => GSmartCopy (M1 D d f) where
    gwriteSmart fmt (M1 x) = gwriteSmart fmt x
    greadSmart fmt = M1 <$> greadSmart fmt

instance (Constructor c, Selectors f, GSmartCopy f) => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x)
        = let fields = getFields $ selectors (P.Proxy :: P.Proxy (M1 C c f))
              cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields
                       (multipleConstructors con)
                       (getIndex con (conName (undefined :: M1 C c f p)))
          in withCons fmt cons $ gwriteSmart fmt x
    greadSmart fmt = undefined

instance (Constructor c, Selector s, GSmartCopy f)
         => GSmartCopy (M1 C c (M1 S s f)) where
    gwriteSmart fmt con@(M1 x)
        = let fields = getFields $ selectors (P.Proxy :: P.Proxy (M1 C c (M1 S s f)))
              cons = C (T.pack $ conName (undefined :: M1 C c f p)) fields
                       (multipleConstructors con)
                       (getIndex con (conName (undefined :: M1 C c f p)))
          in withCons fmt cons $ do putter <- ggetSmartPut fmt
                                    withField fmt $ putter x

instance (Selector s, GSmartCopy f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) = do putter <- ggetSmartPut fmt
                                withField fmt $ putter a
    greadSmart fmt = readField fmt $ greadSmart fmt

instance SmartCopy c => GSmartCopy (K1 a c) where
    gversion = castVersion (versionFromProxy (Proxy :: Proxy c))
    gwriteSmart fmt (K1 a) = writeSmart fmt a
    greadSmart fmt = liftM K1 (readSmart fmt)

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) = gwriteSmart fmt x
    gwriteSmart fmt (R1 x) = gwriteSmart fmt x
    greadSmart fmt = L1 <$> greadSmart fmt
                        <|> R1 <$> greadSmart fmt

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) = do putterA <- ggetSmartPut fmt
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
    selectors :: P.Proxy rep -> [String] ---- TODO: Add indizes for sumtypes

instance Selectors f => Selectors (M1 C c f) where
    selectors _ = selectors (P.Proxy :: P.Proxy f)

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
    selectors _ = undefined --- Fix

instance Selector s => Selectors (M1 S s (K1 R t)) where
    selectors _ =
        [selName (undefined :: M1 S s (K1 R t) ())]

instance Selector s => Selectors (M1 S s f) where
    selectors _ =
        [selName (undefined :: M1 S s f x)]

instance (Selectors a, Selectors b) => Selectors (a :*: b) where
    selectors _ = selectors (P.Proxy :: P.Proxy a) ++ selectors (P.Proxy :: P.Proxy b)

instance Selectors U1 where
    selectors _ = []

getFields :: [String] -> Fields
getFields [] = Empty
getFields sels@(x:xs)
    = case x of
        "" -> NF (length sels)
        string -> LF $ map T.pack sels
    
getConNames :: (Generic a, ConNames (Rep a)) => a -> [T.Text]
getConNames = map T.pack . conNames

getIndex :: (Generic a, ConNames (Rep a)) => a -> String -> Integer
getIndex c name = toInteger $ fromJust $ L.elemIndex name (conNames c)

multipleConstructors :: (Generic a, ConNames (Rep a)) => a -> Bool
multipleConstructors = (> 1) . length . conNames
