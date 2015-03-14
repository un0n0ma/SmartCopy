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
    gwriteSmart fmt _ _ _ _ _ = return $ return (\U1 -> return ())
    greadSmart fmt _ _ = return $ return $ return $ Right U1

instance (GVersion f, Datatype d, Selectors f, GSmartCopy f) => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _ _ _
        = liftM (liftM (\g (M1 x) -> g x)) $ gwriteSmart fmt x False 0 Empty []
    greadSmart fmt _ _
        = liftM (liftM (liftM (fmap M1))) $ greadSmart fmt [] []
         
instance (GVersion f, Constructor c, Selectors f, GSmartCopy f) => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) multCons index fields _
        = do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             fields' <-
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
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x multCons index' fields' types
                  getter <- wrapped
                  getter x
    greadSmart fmt [] _
        = do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             fields <- getFields 0 $ selectors (P.Proxy :: P.Proxy (M1 C c f)) 0
             let con = C (T.pack $ conName (undefined :: M1 C c f p)) fields False 0 True
                 fieldN =
                    case cfields con of
                      Empty -> 0
                      NF x -> x
                      LF xs -> length xs
             wrapped <- greadSmart fmt [] types
             return $ return $
                    liftM (fmap M1) $ readCons fmt [(con, join wrapped)]
    greadSmart fmt conList _
        = do let types = dupRepsToNothing $ gversions (P.Proxy :: P.Proxy f)
             let [con, _empty] = conList
             let fieldN =
                    case cfields con of
                      Empty -> 0
                      NF x -> x
                      LF xs -> length xs
             wrapped <- greadSmart fmt [] types
             return $ return $
                 liftM (fmap M1) $
                     readCons fmt $ zip conList $ repeat $ join wrapped

instance (Selector s, GSmartCopy f, GVersion f) => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) multCons conInd fields types
        = liftM (liftM (\g (M1 a) -> withField fmt $ g a)) $
            gwriteSmart fmt a multCons conInd fields types
    greadSmart fmt _ types
        = liftM (liftM (readField fmt . liftM (fmap M1))) $ greadSmart fmt [] types

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) _ _ _ types
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

instance (ConNames a, ConNames b, Selectors a, Selectors b, GSmartCopy a, GSmartCopy b)
         => GSmartCopy (a :+: b) where
    gwriteSmart fmt (L1 x) _ conInd _ _
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
             liftM (liftM $ \g (L1 x) -> g x) $ gwriteSmart fmt x True conInd fields' []
    gwriteSmart fmt (R1 x) _ conInd _ _
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
             liftM (liftM $ \g (R1 x) -> g x) $ gwriteSmart fmt x True (conInd + 1) fields' []
    greadSmart fmt conList _
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
             liftM2 (liftM2 $ withLookahead fmt cindex1)
                    (liftM (liftM (liftM (fmap L1))) $ greadSmart fmt cList1 [])
                    (liftM (liftM (liftM (fmap R1))) $ greadSmart fmt cList2 [])

instance (GSmartCopy a, GSmartCopy b, GVersion a, GVersion b) => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) multCons conInd fields types
        = case types of
            (x:xs) ->
                liftM2 (liftM2 $ \gA gB (_:*:_) -> gA a >> gB b)
                   (gwriteSmart fmt a multCons conInd fields [x])
                   (gwriteSmart fmt b multCons conInd fields xs)
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
    mkConList :: Monad m => P.Proxy rep -> Integer -> m [Cons]

instance (ConList f, Datatype d) => ConList (M1 D d f) where
    mkConList _ = mkConList (P.Proxy :: P.Proxy f)

instance (Selectors f, ConNames f, ConList f, Constructor c) => ConList (M1 C c f) where
    mkConList _ _ =
        do fields <- getFields 0 $ selectors (P.Proxy :: P.Proxy (M1 C c f)) 0
           let Just i = L.elemIndex (gconNameOf (undefined :: M1 C c f x)) $
                                    gconNames (undefined :: M1 C c f x)
               conInd = fromIntegral i
           return [C (T.pack $ conName (undefined :: M1 C c f p)) fields False conInd True]

instance (ConList a, ConList b) => ConList (a :+: b) where
    mkConList _ conInd
        = liftM2 (++) (mkConList (P.Proxy :: P.Proxy a) conInd) 
                      (mkConList (P.Proxy :: P.Proxy b) (conInd + 1))

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
