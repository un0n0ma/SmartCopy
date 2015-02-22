{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Control.Applicative
import Control.Monad (liftM)
import Data.Maybe
import GHC.Generics
import Generics.Deriving.ConNames

getConNames :: (Generic a, ConNames (Rep a)) => a -> [T.Text]
getConNames = map T.pack . conNames

getIndex :: (Generic a, ConNames (Rep a)) => a -> String -> Integer
getIndex c name = toInteger $ fromJust $ L.elemIndex name (conNames c)

{-
getFields :: (Constructor c, Selector s, Generic a)
          => (M1 C c ((M1 S s f) :*: (M1 S s f)) a)
          -> Fields
getFields = case selName (undefined :: M1 S s f) of
              "" -> NF 1
              _ -> 
              -}

multipleConstructors :: (Generic a, ConNames (Rep a)) => a -> Bool
multipleConstructors = (> 1) . length . conNames

instance GSmartCopy U1 where
    gwriteSmart fmt = \_ -> return ()
    greadSmart fmt = return U1

instance Constructor c => GSmartCopy (M1 C c U1) where
    gwriteSmart fmt (M1 a)
        = withCons fmt cons $ gwriteSmart fmt a
          where cons = C (T.pack $ conName (undefined :: M1 C c U1 f)) Empty
                         (multipleConstructors (M1 a))
                         (getIndex (M1 a) (conName (undefined :: M1 C c f p)))
    greadSmart fmt
        = readCons fmt (zip cons (repeat $ return (undefined :: M1 C c U1 p)))
          where cons = map (\(name, index) ->
                       C name (NF 0) (multipleConstructors (undefined :: M1 C c f p))
                       index) $
                       zip (map T.pack $ conNames (undefined :: M1 C c f p)) [0..]
                       {-

instance (Constructor c, Selector s1, Selector s2, GSmartCopy f) =>
         GSmartCopy (M1 C c ((M1 S s1 f) :*: (M1 S s2 f))) where
    gwriteSmart fmt (M1 (C c (a :*: b)))
        = let fields = case selName (undefined :: M1 S s1 f p) of
                         "" -> NF 1
                         string -> LF [T.pack string]
              cons = C (T.pack $ conName (undefined :: M1 C c (a :*: b) p)) fields
                       (multipleConstructors (M1 a))
                       (getIndex (M1 a) (conName (undefined :: M1 C c (a :*: b) p)))
          in withCons fmt cons $
                 do gwriteSmart fmt a
                    gwriteSmart fmt b
                    -}


instance (Selector s, GSmartCopy f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) = withField fmt $ gwriteSmart fmt a
    greadSmart fmt = readField fmt $ greadSmart fmt

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) = writeSmart fmt a
    greadSmart fmt = liftM K1 (readSmart fmt)

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) = gwriteSmart fmt x
    gwriteSmart fmt (R1 x) = gwriteSmart fmt x
    greadSmart fmt = L1 <$> greadSmart fmt
                        <|> R1 <$> greadSmart fmt

instance (GSmartCopy a, GSmartCopy b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) = do withField fmt $ gwriteSmart fmt a
                                   withField fmt $ gwriteSmart fmt b
    greadSmart fmt = (:*:) <$> (readField fmt $ greadSmart fmt) <*> (readField fmt $ greadSmart fmt)
