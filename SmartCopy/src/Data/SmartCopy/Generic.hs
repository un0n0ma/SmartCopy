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

module Data.SmartCopy.Generic where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy.SmartCopy

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
    greadSmart fmt _ _ = return $ return $ return U1

instance
    (GVersion f, Datatype d, GParserList f, GSelectors f, GSmartCopy f, GConList f)
    => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _
        = liftM (liftM (\g (M1 x) -> g x)) $ gwriteSmart fmt x [] []
    greadSmart fmt _ _
        = return $ return $
            do conList <- mkGConList (P.Proxy :: P.Proxy f) 0
               let conMap = mkGParserList fmt (undefined :: f x)
               liftM M1 $ readCons fmt $ zip conList conMap

instance
    (GVersion f, GConList f, Constructor c, GSelectors f, GSmartCopy f)
    => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) [] _ --- single constructor
        = do let [tyVer'] = gversions (P.Proxy :: P.Proxy f)
                 tyVer = dupRepsToNothing tyVer'
             [cons] <- mkGConList (P.Proxy :: P.Proxy (M1 C c f)) 0
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x [] tyVer
                  putter <- wrapped
                  putter x
    gwriteSmart fmt con@(M1 x) [cons] _ --- sumtype constructor
        = do let [tyVer'] = gversions (P.Proxy :: P.Proxy f)
                 tyVer = dupRepsToNothing tyVer'
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x [] tyVer
                  getter <- wrapped
                  getter x
    greadSmart fmt [] _
        = undefined
    greadSmart fmt conList _
        = undefined

instance
    (Selector s, GSmartCopy f, GVersion f)
    => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) _ tyVer
        = liftM (liftM (\g (M1 a) -> withField fmt $ g a)) $
            gwriteSmart fmt a [] tyVer
    greadSmart fmt _ tyVer
        = liftM (liftM (readField fmt . liftM M1)) $ greadSmart fmt [] tyVer

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) _ tyVer
        = case tyVer of
            [(Nothing,_)] ->
                return $ liftM (\g (K1 x) -> g x) $ mkPutter fmt False $
                         unVersion (version :: Version c)
            [(Just _,_)] ->
                return $ liftM (\g (K1 x) -> g x) $ mkPutter fmt True $
                         unVersion (version :: Version c)
            xs ->
                return $ return $ return $
                mismatch "singleton list with TypeRep of a field value" (show xs)
    greadSmart fmt _ tyVer
        = case tyVer of
            [(Nothing, v)] ->
                 return $ liftM (liftM K1) $ mkGetter fmt False v
            [(Just _, v)] ->
                return $ liftM (liftM K1) $ mkGetter fmt True v
            xs ->
                fail "Was expecting singleton list with TypeRep of a field value at " (show xs)

instance
    (ConNames a, ConNames b, GSmartCopy a, GSmartCopy b, GConList (a :+: b))
    => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) conList _
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             conListL:_ <- mkGConList (P.Proxy :: P.Proxy (a :+: b)) conInd
             liftM (liftM $ \g (L1 x) -> g x) $ gwriteSmart fmt x [conListL] []
    gwriteSmart fmt (R1 x) conList _
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             _:conListR <- mkGConList (P.Proxy :: P.Proxy (a :+: b)) conInd
             liftM (liftM $ \g (R1 x) -> g x) $ gwriteSmart fmt x conListR []
    greadSmart fmt conList _
        = undefined

instance
    (GSmartCopy a, GSmartCopy b, GVersion a, GVersion b)
    => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) _ tyVer
        = case tyVer of
            (x:xs) ->
                liftM2 (liftM2 $ \gA gB (_:*:_) -> gA a >> gB b)
                   (gwriteSmart fmt a [] [x])
                   (gwriteSmart fmt b []  xs)
            f -> return $ return $ return $
                 mismatch "list with TypeReps of field values at " (show f)
    greadSmart fmt conList tyVer
        = case tyVer of
            (x:xs) ->
                liftM2 (liftM2 (liftM2 (:*:)))
                  (greadSmart fmt [] [x])
                  (greadSmart fmt [] xs)
            f ->
                fail ("Was expecting list with TypeReps of field values at " ++ show f)


-------------------------------------------------------------------------------
-- Helper functions/types for accessing selector and constructor information
-------------------------------------------------------------------------------

class GConList (rep :: * -> *) where
    mkGConList :: Monad m => P.Proxy rep -> Integer -> m [ConstrInfo]

instance (GConList f, Datatype d) => GConList (M1 D d f) where
    mkGConList _ = mkGConList (P.Proxy :: P.Proxy f)

instance (GSelectors f, GConList f, Constructor c) => GConList (M1 C c f) where
    mkGConList _ _ =
        do let Just i = L.elemIndex (gconNameOf (undefined :: M1 C c f x)) $
                                    gconNames (undefined :: M1 C c f x)
               conInd = fromIntegral i
           fields <- getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f)) conInd
           return [CInfo (T.pack $ conName (undefined :: M1 C c f p)) fields False conInd]

instance
    (GConList f, GConList g, GSelectors f, GSelectors g, Constructor c1, Constructor c2)
    => GConList (M1 C c1 f  :+: M1 C c2 g) where
    mkGConList _ conInd
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

instance
    (GConList f, GSelectors f, GSelectors b, Constructor c, GConList b)
    => GConList (M1 C c f :+: b) where
    mkGConList _ conInd
        = do fields1 <-
                 getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f :+: b)) conInd
             consB <-
                mkGConList (P.Proxy :: P.Proxy b) (conInd + 1)
             let fieldsB = map cfields consB
                 f1
                   | fields1 == Empty && (filter (== Empty) fieldsB == fieldsB)
                   = Empty
                   | fields1 == Empty
                   = NF 0
                   | otherwise
                   = fields1
             return $ CInfo (T.pack $ conName (undefined :: M1 C c f p)) f1 True conInd : consB

instance (GConList f, Selector s) => GConList (M1 S s f) where
    mkGConList _ = mkGConList (P.Proxy :: P.Proxy f)

instance (GConList a, GConList b) => GConList (a :*: b) where
    mkGConList _ conInd
        = liftM2 (++) (mkGConList (P.Proxy :: P.Proxy a) conInd)
                      (mkGConList (P.Proxy :: P.Proxy b) conInd)

instance GConList U1 where
    mkGConList _ _ = return []

instance GConList (K1 R t) where
    mkGConList _ _ = return []

-------------------------------------------------------------------------------
class GSelectors (rep :: * -> *) where
    selectors :: P.Proxy rep -> Integer -> [(Integer, [String])]

instance (GSelectors f, Datatype d) => GSelectors (M1 D d f) where
    selectors proxy = selectors (P.Proxy :: P.Proxy f)

instance (GSelectors f, Constructor c) => GSelectors (M1 C c f) where
    selectors proxy = selectors (P.Proxy :: P.Proxy f)

instance (GSelectors a, GSelectors b) => GSelectors (a :+: b) where
    selectors proxy conInd =
        selectors (P.Proxy :: P.Proxy a) conInd ++
        selectors (P.Proxy :: P.Proxy b) (conInd + 1)

instance Selector s => GSelectors (M1 S s (K1 R t)) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s (K1 R t) ())])]

instance GSelectors (K1 R t) where
    selectors _ conInd = []

instance Selector s => GSelectors (M1 S s f) where
    selectors _ conInd =
        [(conInd, [selName (undefined :: M1 S s f x)])]

instance (GSelectors a, GSelectors b) => GSelectors (a :*: b) where
    selectors _ conInd =
        let sels = map snd (selectors (P.Proxy :: P.Proxy a) conInd) ++
                   map snd (selectors (P.Proxy :: P.Proxy b) conInd)
        in [(conInd, concat sels)]

instance GSelectors U1 where
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
    gversions :: P.Proxy rep -> [[(TypeRep, Int32)]]
    
instance (GVersion f, Datatype d) => GVersion (M1 D d f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (SmartCopy c, Typeable c, Generic c, ConNames (Rep c)) => GVersion (K1 a c) where
    gversions _ = [[(typeOf (undefined :: c), unVersion (version :: Version c))]]

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
        let [selL] = gversions (P.Proxy :: P.Proxy a)
            [selRest] = gversions (P.Proxy :: P.Proxy b)
        in [selL ++ selRest]

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
class GParserList f where
    mkGParserList :: (Applicative m, Monad m, Alternative m, GSelectors f)
                   => ParseFormat m -> f a -> [m (f a)]

instance
    (Constructor c, GSmartCopy f, GSelectors f, GSelectors g, GParserList g, GVersion f, GVersion g)
    => GParserList (M1 C c f :+: g) where
    mkGParserList fmt _ =
        let tyVer
              = case gversions (P.Proxy :: P.Proxy (M1 C c f :+: g)) of
                  [] -> []
                  tyVer':_ -> dupRepsToNothing tyVer'
        in
        liftM (L1 . M1) (join $ join $ greadSmart fmt [] tyVer)
        : map (liftM R1) (mkGParserList fmt (undefined :: g x))

instance
    (Constructor c, GConList f, GSmartCopy f, GSelectors f, GVersion f)
    => GParserList (M1 C c f) where
    mkGParserList fmt _ =
        let [tyVer'] = gversions (P.Proxy :: P.Proxy (M1 C c f))
            tyVer = dupRepsToNothing tyVer'
        in
        [liftM M1 (join $ join $ greadSmart fmt [] tyVer)]
