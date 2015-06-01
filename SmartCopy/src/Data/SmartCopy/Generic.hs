{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Derive SmartCopy instances using the Generics package and the 
-- DefaultSignatures and DeriveGeneric extensions.
module Data.SmartCopy.Generic () where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import Data.SmartCopy.SmartCopy

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Int
import Data.Tree
import Text.Parsec (try)

import qualified Data.List as L
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Data hiding (Proxy)
import Data.Typeable hiding (Proxy)
import Data.Maybe
import GHC.Generics
import Generics.Deriving.ConNames

import qualified Data.Proxy as P

-------------------------------------------------------------------------------
-- Rep instances
-------------------------------------------------------------------------------
instance GSmartCopy U1 where
    gwriteSmart fmt _ _ _ _ _ = return $ return (\U1 -> return ())
    greadSmart fmt _ _ _ = return $ return $ return U1

instance
    (GVersion f, Datatype d, GParserList f, GSelectors f, GSmartCopy f, GConList f)
    => GSmartCopy (M1 D d f) where
    gwriteSmart fmt d@(M1 x) _ _ [ident] mIds
        = liftM (liftM (\g (M1 x) -> g x)) $ gwriteSmart fmt x [] [] [ident] mIds
    greadSmart fmt _ _ [ident]
        = return $ return $
            do conList <- mkGConList (P.Proxy :: P.Proxy f) 0 ident
               parserList <- mkGParserList fmt (undefined :: f x) Nothing
               liftM M1 $ readCons fmt $ zip conList parserList

instance
    ( GVersion f, GConList f, Constructor c, GSelectors f, GSmartCopy f
    , GGetIdentifier f )
    => GSmartCopy (M1 C c f) where
    gwriteSmart fmt con@(M1 x) [] _ [id] mIds --- single constructor
        = do [tyVer'] <- gversions (P.Proxy :: P.Proxy f) mIds
             [selIds] <- ggetId (P.Proxy :: P.Proxy f) mIds
             [cons] <- mkGConList (P.Proxy :: P.Proxy (M1 C c f)) 0 id
             let tyVer = dupRepsToNothing tyVer'
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x [] tyVer selIds mIds
                  putter <- wrapped
                  putter x
    gwriteSmart fmt con@(M1 x) [cons] _ _ mIds --- sumtype constructor
        = do [tyVer'] <- gversions (P.Proxy :: P.Proxy f) mIds
             [selIds] <- ggetId (P.Proxy :: P.Proxy f) mIds
             let tyVer = dupRepsToNothing tyVer'
             return $ return $ \(M1 x) -> withCons fmt cons $
               do wrapped <- gwriteSmart fmt x [] tyVer selIds mIds
                  getter <- wrapped
                  getter x
    greadSmart fmt [] _ _
        = undefined
    greadSmart fmt conList _ _
        = undefined

instance
    (Selector s, GSmartCopy f, GVersion f)
    => GSmartCopy (M1 S s f) where
    gwriteSmart fmt (M1 a) _ tyVer [ident] mIds
        = liftM (liftM (\g (M1 a) -> withField fmt $ g a)) $
            gwriteSmart fmt a [] tyVer [ident] mIds
    greadSmart fmt _ tyVer [ident]
        = liftM (liftM (readField fmt . liftM M1)) $ greadSmart fmt [] tyVer [ident]

instance SmartCopy c => GSmartCopy (K1 a c) where
    gwriteSmart fmt (K1 a) _ tyVer _ mIds
        = let putter =
                  case mIds of
                    Just allIds ->
                        \isDup prevVer -> ggetSmartPutLastKnown fmt isDup prevVer allIds
                    Nothing -> ggetSmartPut fmt
          in
          case tyVer of
            [(Nothing, ver)] ->
                return $ liftM (\g (K1 x) -> g x) $ putter False ver
            [(Just _, ver)] ->
                return $ liftM (\g (K1 x) -> g x) $ putter True ver
            xs ->
                return $ return $ return $
                mismatch "singleton list with TypeRep of a field value" (show xs)
    greadSmart fmt _ tyVer _
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
    gwriteSmart fmt (L1 x) conList _ [ident] mIds
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             conListL:_ <- mkGConList (P.Proxy :: P.Proxy (a :+: b)) conInd ident
             liftM (liftM $ \g (L1 x) -> g x) $ gwriteSmart fmt x [conListL] [] [ident] mIds
    gwriteSmart fmt (R1 x) conList _ [ident] mIds
        = do let conInd
                   | null conList
                   = 0
                   | otherwise
                   = toInteger $ length conList - 1
             _:conListR <- mkGConList (P.Proxy :: P.Proxy (a :+: b)) conInd ident
             liftM (liftM $ \g (R1 x) -> g x) $ gwriteSmart fmt x conListR [] [ident] mIds
    greadSmart fmt conList _ _
        = undefined

instance
    (GSmartCopy a, GSmartCopy b, GVersion a, GVersion b)
    => GSmartCopy (a :*: b) where
    gwriteSmart fmt (a :*: b) _ tyVer (id:ids) mIds
        = case tyVer of
            (x:xs) ->
                liftM2 (liftM2 $ \gA gB (_:*:_) -> gA a >> gB b)
                   (gwriteSmart fmt a [] [x] [id] mIds)
                   (gwriteSmart fmt b []  xs ids mIds)
            f -> return $ return $ return $
                 mismatch "list with TypeReps of field values at " (show f)
    greadSmart fmt conList tyVer (id:ids)
        = case tyVer of
            (x:xs) ->
                liftM2 (liftM2 (liftM2 (:*:)))
                  (greadSmart fmt [] [x] [id])
                  (greadSmart fmt [] xs ids)
            f ->
                fail ("Was expecting list with TypeReps of field values at " ++ show f)

-------------------------------------------------------------------------------
-- Helper functions/types for accessing selector and constructor information
-------------------------------------------------------------------------------

class GConList (rep :: * -> *) where
    mkGConList :: Monad m => P.Proxy rep -> Integer -> String -> m [ConstrInfo]

instance (GConList f, Datatype d) => GConList (M1 D d f) where
    mkGConList _ = mkGConList (P.Proxy :: P.Proxy f)

instance (GSelectors f, GConList f, Constructor c) => GConList (M1 C c f) where
    mkGConList _ _ id =
        do let Just i = L.elemIndex (gconNameOf (undefined :: M1 C c f x)) $
                                    gconNames (undefined :: M1 C c f x)
               conInd = fromIntegral i
           fields <- getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f)) conInd
           return [CInfo (T.pack $ conName (undefined :: M1 C c f p)) fields False conInd id]

instance
    (GConList f, GConList g, GSelectors f, GSelectors g, Constructor c1, Constructor c2)
    => GConList (M1 C c1 f  :+: M1 C c2 g) where
    mkGConList _ conInd id
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
             return [ CInfo (T.pack $ conName (undefined :: M1 C c1 f p)) f1 True conInd id
                    , CInfo (T.pack $ conName (undefined :: M1 C c2 g p)) f2 True (conInd + 1) id
                    ]

instance
    ( GConList f, GSelectors f, GSelectors b, Constructor c, GConList b
    , GGetIdentifier f )
    => GConList (M1 C c f :+: b) where
    mkGConList _ conInd ident
        = do fields1 <-
                 getFields conInd $ selectors (P.Proxy :: P.Proxy (M1 C c f :+: b)) conInd
             consB <-
                mkGConList (P.Proxy :: P.Proxy b) (conInd + 1) ident
             let fieldsB = map cfields consB
                 f1
                   | fields1 == Empty && (filter (== Empty) fieldsB == fieldsB)
                   = Empty
                   | fields1 == Empty
                   = NF 0
                   | otherwise
                   = fields1
             return $ CInfo (T.pack $ conName (undefined :: M1 C c f p)) f1 True conInd ident : consB

instance (GConList f, Selector s) => GConList (M1 S s f) where
    mkGConList _ = mkGConList (P.Proxy :: P.Proxy f)

instance (GConList a, GConList b) => GConList (a :*: b) where
    mkGConList _ conInd id
        = liftM2 (++) (mkGConList (P.Proxy :: P.Proxy a) conInd id)
                      (mkGConList (P.Proxy :: P.Proxy b) conInd id)

instance GConList U1 where
    mkGConList _ _ _ = return []

instance GConList (K1 R t) where
    mkGConList _ _ _ = return []

-------------------------------------------------------------------------------
-- |The GSelectors class enables access to all fields of a datatype, sorted
-- by constructor index.
class GSelectors (rep :: * -> *) where
    -- |Returns a map containing the fields of all of a datatype's constructors
    -- sorted by index. Fields are given by a String list with their names or
    -- empty Strings if unlabeled.
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

-- |Given a map with the fields of all of a datatype's constructors and
-- a constructor index, looks up and returns the fields of that constructor
-- as a Field type.
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

-- |The GVersion class enables access to the types and versions of
-- a constructors fields.
class GVersion (rep :: * -> *) where
    -- |Returns a map containing the versions of all fields within a
    -- constructor and their type representations to eliminate duplicate
    -- types from version tagging if wished.
    gversions :: Monad m => P.Proxy rep -> Maybe [String] -> m [[(TypeRep, Int32)]]
    
instance (GVersion f, Datatype d) => GVersion (M1 D d f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (SmartCopy c, Typeable c, Generic c, ConNames (Rep c)) => GVersion (K1 a c) where
    gversions _ mIds =
        case mIds of
          Nothing ->
              return [[(typeOf (undefined :: c), thisVer)]]
          Just allIds ->
              do ver <- getPrevVersion aProxy allIds
                 return [[(typeOf (undefined :: c), ver)]]
        where aProxy = Proxy :: Proxy c
              thisId = unId (identFromProxy aProxy)
              thisVer = unVersion (versionFromProxy aProxy)

getPrevVersion :: (SmartCopy a, Monad m)
               => Proxy a
               -> [String]
               -> m Int32
getPrevVersion aProxy allIds
    = case kindFromProxy aProxy of
        Primitive -> 
            return thisVer
        Base ->
            if thisId `elem` allIds
               then return thisVer
               else fail (idNotFoundPutter thisId allIds)
        Extends bProxy ->
            if thisId `elem` allIds
               then return thisVer
               else getPrevVersion bProxy allIds
      where thisId = unId (identFromProxy aProxy)
            thisVer = unVersion (versionFromProxy aProxy)

instance (GVersion f, Constructor c) => GVersion (M1 C c f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (GVersion a, GVersion b) => GVersion (a :+: b) where
    gversions _ mIds =
        liftM2 (++) (gversions (P.Proxy :: P.Proxy a) mIds) (gversions (P.Proxy :: P.Proxy b) mIds)

instance (GVersion f, Selector s) => GVersion (M1 S s f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance GVersion f => GVersion (M1 S NoSelector f) where
    gversions _ = gversions (P.Proxy :: P.Proxy f)

instance (GVersion a, GVersion b) => GVersion (a :*: b) where
    gversions _ mIds =
        do [selL] <- gversions (P.Proxy :: P.Proxy a) mIds
           [selRest] <- gversions (P.Proxy :: P.Proxy b) mIds
           return [selL ++ selRest]

instance GVersion U1 where
    gversions _ _ = return [[]]

dupRepsToNothing xs = dupRepsToNothing' xs []
dupRepsToNothing' [] ls = []
dupRepsToNothing' ((x,v):xs) ls
  | Just x `elem` ls = (Nothing, v):dupRepsToNothing' xs ls
  | otherwise = (Just x, v):dupRepsToNothing' xs (Just x:ls)

-------------------------------------------------------------------------------
-- Helper functions/types to convert constructor list f_0 :+: f_1 :+: ... f_n
-- into value level list of forall a. [(Cons, m (t a))]
-------------------------------------------------------------------------------

-- |The GParserList class enables access to the parsers of all a datatype's
-- constructors in the D1 instance.
class GParserList f where
    -- |mkGParserList returns a list of parsers for the constructors of a
    -- datatype making it possible to determine which constructor of a sumtype
    -- should be parsed in the D1 instance.
    mkGParserList :: (Applicative m, Monad m, Alternative m, GSelectors f)
                  => ParseFormat m
                  -> f a
                  -> Maybe [String]
                  -> m [m (f a)]

instance
    ( Constructor c, GSmartCopy f, GSelectors f, GSelectors g, GParserList g
    , GVersion f, GVersion g, GGetIdentifier f, GGetIdentifier g )
    => GParserList (M1 C c f :+: g) where
    mkGParserList fmt _ mIds =
        do versions <- gversions (P.Proxy :: P.Proxy (M1 C c f :+: g)) mIds
           let tyVer = case versions of
                         [] -> []
                         tyVer':_ -> dupRepsToNothing tyVer'
           [idsL] <- ggetId (P.Proxy :: P.Proxy f) mIds
           parserListR <- mkGParserList fmt (undefined :: g x) mIds
           return $
               liftM (L1 . M1) (join $ join $ greadSmart fmt [] tyVer idsL)
               : map (liftM R1) parserListR

instance
    ( Constructor c, GConList f, GSmartCopy f, GSelectors f, GVersion f
    , GGetIdentifier f )
    => GParserList (M1 C c f) where
    mkGParserList fmt _ mIds =
        do [tyVer'] <- gversions (P.Proxy :: P.Proxy (M1 C c f)) mIds
           [ids] <- ggetId (P.Proxy :: P.Proxy f) mIds
           [ids] <- ggetId (P.Proxy :: P.Proxy f) mIds
           let tyVer = dupRepsToNothing tyVer'
           return [liftM M1 (join $ join $ greadSmart fmt [] tyVer ids)]

-------------------------------------------------------------------------------
-- Helper functions/types to fetch the Identifier for a SmartCopy instance
-------------------------------------------------------------------------------

-- TODO
class GGetIdentifier (rep :: * -> *) where
    ggetId :: Monad m => P.Proxy rep -> Maybe [String] -> m [[String]]

instance
    (Constructor c, GGetIdentifier f) =>  GGetIdentifier (M1 C c f) where
    ggetId _ = ggetId (P.Proxy :: P.Proxy f)
     
instance
    (Selector s, GGetIdentifier f) =>  GGetIdentifier (M1 S s f) where
    ggetId _ = ggetId (P.Proxy :: P.Proxy f)

instance
    (Datatype d, GGetIdentifier f) =>  GGetIdentifier (M1 D d f) where
    ggetId _ = ggetId (P.Proxy :: P.Proxy f)
              
instance
    (GGetIdentifier a, GGetIdentifier b) =>  GGetIdentifier (a :+: b) where
    ggetId _ mIds =
        liftM2 (++) (ggetId (P.Proxy :: P.Proxy a) mIds) (ggetId (P.Proxy :: P.Proxy b) mIds)

instance
    (GGetIdentifier a, GGetIdentifier b) =>  GGetIdentifier (a :*: b) where
    ggetId _ mIds =
        do [idSel] <- ggetId (P.Proxy :: P.Proxy a) mIds
           [idRest] <- ggetId (P.Proxy :: P.Proxy b) mIds
           return [idSel ++ idRest]

instance SmartCopy c => GGetIdentifier (K1 a c) where
    ggetId proxy mIds =
        checkConsistency aProxy $
        case mIds of
          Nothing ->
              return [[thisId]]
          Just allIds ->
              getPrevId aProxy allIds
        where aProxy = Proxy :: Proxy c
              thisId = unId (identFromProxy aProxy)

getPrevId :: (SmartCopy a, Monad m) => Proxy a -> [String] -> m [[String]]
getPrevId aProxy allIds
    = case thisKind of
        Primitive -> 
            return [[thisId]]
        Base ->
            if thisId `elem` allIds
               then return [[thisId]]
               else fail (idNotFoundPutter thisId allIds)
        Extends bProxy ->
            if thisId `elem` allIds
               then return [[thisId]]
               else getPrevId bProxy allIds
      where thisId = unId (identFromProxy aProxy)
            thisKind = kindFromProxy aProxy

instance GGetIdentifier U1 where
    ggetId _ _ = return [[]]

-------------------------------------------------------------------------------
-- Generic functions for back-migration (with added parameters)
-------------------------------------------------------------------------------
ggetSmartPutLastKnown :: forall a m. (SmartCopy a, Monad m)
                      => SerializationFormat m
                      -> Bool
                      -> Int32
                      -> [String]
                      -> m (a -> m ())
ggetSmartPutLastKnown fmt isDup prevVer allIds =
    checkConsistency aProxy $
    case kindFromProxy aProxy of
      Primitive -> return $ \a -> writeSmart fmt a (Just allIds)
      Base ->
          if thisId `elem` allIds
             then mkPutter fmt isDup prevVer (Just allIds)
             else fail (idNotFoundPutter thisId allIds)
      Extends bProxy ->
          if thisId `elem` allIds
             then mkPutter fmt isDup prevVer (Just allIds)
             else liftM (. migrateBack) (ggetSmartPutLastKnown fmt isDup prevVer allIds)
      where aProxy = Proxy :: Proxy a
            thisId = unId (identFromProxy aProxy)

ggetSmartPut :: forall a m. (SmartCopy a, Monad m)
             => SerializationFormat m
             -> Bool
             -> Int32
             -> m (a -> m ())
ggetSmartPut fmt isDup prevVer =
    checkConsistency proxy $
    case kindFromProxy proxy of
      Primitive -> return $ \a -> writeSmart fmt (asProxyType a proxy) Nothing
      _ -> mkPutter fmt isDup prevVer Nothing
    where proxy = Proxy :: Proxy a
