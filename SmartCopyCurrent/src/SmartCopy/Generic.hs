{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

import Data.Int
import Data.Tree
import Text.Parsec (try)

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import qualified Data.Proxy as P

import Control.Applicative
import Control.Arrow ((***))
import Control.Monad
import Data.Data hiding (Proxy)
import Data.Typeable hiding (Proxy)
import Data.Maybe
import GHC.Generics
import Generics.Deriving.ConNames

-------------------------------------------------------------------------------
-- Rep instances
-------------------------------------------------------------------------------
instance GSmartCopy U1 where
    gwriteSmart fmt _ _ _ = return $ return (\U1 -> return ())
    greadSmart fmt _ _ = return $ return $ return $ Right U1

instance
    (GVersion f, Datatype d, ConParserMap f, Selectors f, GSmartCopy f)
    => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _
        = liftM (liftM (\g (M1 x) -> g x)) $ gwriteSmart fmt x [] []
    greadSmart fmt _ _
        = return $ return $
            do conList <- mkConList fmt (P.Proxy :: P.Proxy f) 0
               conMap <- mkConParserMap fmt (undefined :: f x) 0
               liftM (fmap M1) $ readCons fmt conMap

instance (GVersion f, ConList f, Constructor c, Selectors f, GSmartCopy f) => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) [] _ --- single constructor
        = liftM (liftM (\g (M1 x) -> g x)) $ gwriteSmart fmt x [] []
    gwriteSmart fmt con@(M1 x) [cons] _ --- sumtype constructor
        = do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x [] types
                  getter <- wrapped
                  getter x
    greadSmart fmt [] _
        = undefined {- do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             [cons] <- mkConList (P.Proxy :: P.Proxy (M1 C c f)) 0
             wrapped <- greadSmart fmt [] types
             return $ return $
                    liftM (fmap M1) $ readCons fmt [(cons, join wrapped)] -}
    greadSmart fmt conList _
        = undefined {-do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             wrapped <- greadSmart fmt [] types
             return $ return $
                 liftM (fmap M1) $
                     readCons fmt $ zip (conList++[emptyCons]) $ repeat $ join wrapped
                     -}

instance (Selector s, GSmartCopy f, GVersion f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) _ types
        = liftM (liftM (\g (M1 a) -> withField fmt $ g a)) $
            gwriteSmart fmt a [] types
    greadSmart fmt _ types
        = liftM (liftM (readField fmt . liftM (fmap M1))) $ greadSmart fmt [] types

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) _ types
        = case types of
            [(Nothing,_)] ->
                return $ liftM (\g (K1 x) -> g x) $ mkPutter fmt False $
                         unVersion (version :: Version c)
            [(Just _,_)] ->
                return $ liftM (\g (K1 x) -> g x) $ mkPutter fmt True $
                         unVersion (version :: Version c)
            xs ->
                return $ return $ return $
                mismatchFail "singleton list with TypeRep of a field value" (show xs)
    greadSmart fmt _ types
        = case types of
            [(Nothing, v)] ->
                 return $ liftM (liftM (fmap K1)) $ mkGetter fmt False v
            [(Just _, v)] ->
                return $ liftM (liftM (fmap K1)) $ mkGetter fmt True v
            xs ->
                fail "Was expecting singleton list with TypeRep of a field value at " (show xs)

instance (ConNames a, ConNames b, Selectors a, Selectors b, GSmartCopy a, GSmartCopy b, ConList (a :+: b))
         => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) conList _
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             conListL:_ <- mkConList (P.Proxy :: P.Proxy (a :+: b)) conInd
             liftM (liftM $ \g (L1 x) -> g x) $ gwriteSmart fmt x [conListL] []
    gwriteSmart fmt (R1 x) conList _
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             _:conListR <- mkConList (P.Proxy :: P.Proxy (a :+: b)) conInd
             liftM (liftM $ \g (R1 x) -> g x) $ gwriteSmart fmt x conListR []
    greadSmart fmt conList _
        = undefined {-do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             conListL:conListR <- mkConList (P.Proxy :: P.Proxy (a :+: b)) conInd
             liftM2 (liftM2 $ withLookahead fmt conInd)
                    (liftM (liftM (liftM (fmap L1))) $ greadSmart fmt [conListL] [])
                    (liftM (liftM (liftM (fmap R1))) $ greadSmart fmt conListR [])
                    -}

instance (GSmartCopy a, GSmartCopy b, GVersion a, GVersion b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) _ types
        = case types of
            (x:xs) ->
                liftM2 (liftM2 $ \gA gB (_:*:_) -> gA a >> gB b)
                   (gwriteSmart fmt a [] [x])
                   (gwriteSmart fmt b []  xs)
            f -> return $ return $ return $
                 mismatchFail "list with TypeReps of field values" (show f)
    greadSmart fmt conList types
        = case types of
            (x:xs) ->
                liftM2 (liftM2 (liftM2 (liftM2 (:*:))))
                  (greadSmart fmt [] [x])
                  (greadSmart fmt [] xs)
            f ->
                fail ("Was expecting list with TypeReps of field values" ++ show f)


-------------------------------------------------------------------------------
-- Helper functions/types for accessing selector and constructor information
-------------------------------------------------------------------------------

class ConList (rep :: * -> *) where
    mkConList :: Monad m => P.Proxy rep -> Integer -> m [ConstrInfo]

instance (ConList f, Datatype d) => ConList (M1 D d f) where
    mkConList _ = mkConList (P.Proxy :: P.Proxy f)

instance (Selectors f, ConList f, Constructor c) => ConList (M1 C c f) where
    mkConList _ _ =
        do let Just i = L.elemIndex (gconNameOf (undefined :: M1 C c f x)) $
                                    gconNames (undefined :: M1 C c f x)
               conInd = fromIntegral i
           fields <- getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f)) conInd
           return [CInfo (T.pack $ conName (undefined :: M1 C c f p)) fields False conInd]

instance (ConList f, ConList g, Selectors f, Selectors g, Constructor c1, Constructor c2) => ConList (M1 C c1 f  :+: M1 C c2 g) where
    mkConList _ conInd
        = do fields1 <-
                 getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c1 f :+: M1 C c2 g)) conInd
             fields2 <-
                 getFields (conInd + 1) $ selectors (P.Proxy :: P.Proxy (M1 C c1 g :+: M1 C c2 g)) conInd
             let f1
                   | fields1 == Empty && fields2 == Empty
                   = Empty
                   | fields1 == Empty && fields2 /= Empty
                   = NF 0
                   | otherwise
                   = fields1
                 f2
                   | fields2 == Empty && fields1 == Empty
                   = Empty
                   | fields2 == Empty && fields1 /= Empty
                   = NF 0
                   | otherwise
                   = fields2
             return [ CInfo (T.pack $ conName (undefined :: M1 C c1 f p)) f1 True conInd
                    , CInfo (T.pack $ conName (undefined :: M1 C c2 g p)) f2 True (conInd + 1)
                    ]

instance (ConList f, Selectors f, Selectors b, Constructor c, ConList b) =>
         ConList (M1 C c f :+: b) where
    mkConList _ conInd
        = do fields1 <-
                 getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f :+: b)) conInd
             consB <-
                mkConList (P.Proxy :: P.Proxy b) (conInd + 1)
             let fieldsB = map cfields consB
                 f1
                   | fields1 == Empty && (filter (== Empty) fieldsB == fieldsB)
                   = Empty
                   | fields1 == Empty
                   = NF 0
                   | otherwise
                   = fields1
             return $ CInfo (T.pack $ conName (undefined :: M1 C c f p)) f1 True conInd : consB

instance (ConList f, Selector s) => ConList (M1 S s f) where
    mkConList _ = mkConList (P.Proxy :: P.Proxy f)

instance (ConList a, ConList b) => ConList (a :*: b) where
    mkConList _ conInd
        = liftM2 (++) (mkConList (P.Proxy :: P.Proxy a) conInd)
                      (mkConList (P.Proxy :: P.Proxy b) conInd)

instance ConList U1 where
    mkConList _ _ = return []

instance ConList (K1 R t) where
    mkConList _ _ = return []

-------------------------------------------------------------------------------
class Selectors (rep :: * -> *) where
    selectors :: P.Proxy rep -> Integer -> [(Integer, [String])]

instance (Selectors f, Datatype d) => Selectors (M1 D d f) where
    selectors proxy = selectors (P.Proxy :: P.Proxy f)

instance (Selectors f, Constructor c) => Selectors (M1 C c f) where
    selectors proxy = selectors (P.Proxy :: P.Proxy f)

instance (Selectors a, Selectors b) => Selectors (a :+: b) where
    selectors proxy conInd =
        selectors (P.Proxy :: P.Proxy a) conInd ++
        selectors (P.Proxy :: P.Proxy b) (conInd + 1)

instance Selector s => Selectors (M1 S s (K1 R t)) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s (K1 R t) ())])]

instance Selectors (K1 R t) where
    selectors _ conInd = []

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
                   show conInd ++ " in " ++ show sels
    
getConNames :: (Generic a, ConNames (Rep a)) => a -> [T.Text]
getConNames = map T.pack . conNames

-------------------------------------------------------------------------------
-- Helper functions/types for accessing versions of SmartCopy instances
-------------------------------------------------------------------------------

class GVersion (rep :: * -> *) where
    gversions :: P.Proxy rep -> [(TypeRep, Int32)]
    
instance (GVersion f, Datatype d) => GVersion (M1 D d f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (SmartCopy c, Typeable c, Generic c, ConNames (Rep c)) => GVersion (K1 a c) where
    gversions _ = [(typeOf (undefined :: c), unVersion (version :: Version c))]

instance (GVersion f, Constructor c) => GVersion (M1 C c f) where
    gversions _ =
        gversions (P.Proxy :: P.Proxy f)

instance (GVersion a, GVersion b) => GVersion (a :+: b) where
    gversions _ =
        gversions (P.Proxy :: P.Proxy a) ++ gversions (P.Proxy :: P.Proxy b)

instance (GVersion f, Selector s) => GVersion (M1 S s f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance GVersion f => GVersion (M1 S NoSelector f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (GVersion a, GVersion b) => GVersion (a :*: b) where
    gversions _ =
        gversions (P.Proxy :: P.Proxy a) ++
        gversions (P.Proxy :: P.Proxy b)

instance GVersion U1 where
    gversions _ = []

dupRepsToNothing xs = dupRepsToNothing' xs []
dupRepsToNothing' [] ls = []
dupRepsToNothing' ((x,v):xs) ls
  | Just x `elem` ls = (Nothing, v):dupRepsToNothing' xs ls
  | otherwise = (Just x, v):dupRepsToNothing' xs (Just x:ls)

-------------------------------------------------------------------------------
-- Helper functions/types to convert constructor list f_0 :+: f_1 :+: ... f_n
-- into value level list of forall a. [(Cons, m (t a))]
-------------------------------------------------------------------------------
class ConParserMap f where
    mkConParserMap :: (Applicative m, Monad m, Alternative m, Selectors f)
                   => ParseFormat m -> f a -> Integer -> m [(ConstrInfo, m (Either String (f a)))]

instance
    (Constructor c, ConList g, ConList f, GSmartCopy f, Selectors f, Selectors g, ConParserMap g, GVersion f)
    => ConParserMap (M1 C c f :+: g) where
    mkConParserMap fmt _ conInd =
       do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
          consL':_ <- mkConList (P.Proxy :: P.Proxy (M1 C c f)) conInd
          let consL = consL' { ctagged = True, cindex = conInd }
          consMapR <- mkConParserMap fmt (undefined :: g a) (conInd + 1)
          return $
            ( consL, liftM (fmap (L1 . M1)) (join $ join $ greadSmart fmt [] types))
            : map (id *** liftM (fmap R1)) consMapR

instance
    (Constructor c, ConList f, GSmartCopy f, Selectors f, GVersion f)
    => ConParserMap (M1 C c f) where
    mkConParserMap fmt _ conInd =
        do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
           [cons] <- mkConList (P.Proxy :: P.Proxy (M1 C c f)) (conInd + 1)
           return [(cons, liftM (fmap M1) (join $ join $ greadSmart fmt [] types))]
