{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module SmartCopy where

import Control.Monad
import Control.Applicative
import qualified Data.Vector as V
import Data.Monoid
import GHC.Generics
import Data.Data hiding (Proxy)



------- TODOs: ----------------------------------------------------------------
{-
* Datatype-generic implementation of parser (possibly need Data.Data?)
* Support arrays, sumtypes and recursive calls with parser
* Version control (possibly need to parametrize EncodedType, have tried some things,
  no solution yet.
* There is still a bug in Generic encoding that has to be fixed (something with
  a first array element missing, not occuring with hand-written instances)
* Move code to proper places (e.g. Parser code at the end of this file)
)
-}

-------------------------------------------------------------------------------
-- SafeCopy types
-------------------------------------------------------------------------------
data Kind a where
    Primitive :: Kind a
    Base :: Kind a
    Extension :: (Migrate a) => Proxy (MigrateFrom a) -> Kind a

class Migrate a where
    type MigrateFrom a
    migrate :: MigrateFrom a -> a

data Version a = Version Int

data Proxy a = Proxy


-------------------------------------------------------------------------------
-- Format class
-------------------------------------------------------------------------------

class (Monoid (m EncodedType), Functor m, Applicative m, Monad m) => Format m where
    type EncodedType :: *
    type EncodedTypeVersioned :: *
    unPack :: m a -> a
    returnEmpty :: m EncodedType

------ Encoding
    -- enterCons :: CT -> m ()
    enterCons :: CT -> m EncodedType -> m EncodedType
    enterField :: FT -> Bool -> m EncodedType -> m EncodedType --- Bool: multiple fields
    openRepetition :: Int -> [m EncodedType] -> m EncodedType --- Int for list length (optional)
    writeValue :: Prim -> m EncodedType
    closeRepetition :: m EncodedType
    leaveField :: m () --  probably only needed for XML
    leaveCons :: m ()
    mult :: m EncodedType -> m EncodedType -> m EncodedType

------ Parsing
    -- Lookup constructor of sumtype and construct getter accordingly. 
    -- Need a better idea, this is not going to
    -- work with generics I think.
    --- getFromCons is right now only used to look up constructors of sumtypes.
    getFromCons :: CT -> m EncodedType -> m (Parser Prim)
    parseField :: FT -> [FT] -> m EncodedType -> m (Parser Prim)
    parseRepetition :: [FT] -> m EncodedType -> [m (Parser Prim)] --- needs fixing
    parseValue :: m EncodedType -> m (Parser Prim)


-------------------------------------------------------------------------------
-- Constructor, field, primitive types
-------------------------------------------------------------------------------
data CFVType = CT
             | FT
             | Prim

data CT
    = Cons
    { ct_index :: Int -- Zero for non-sumtypes. Probably needed for Generic instances.
    , ct_level :: Int -- Zero for outer constructor (object).
    , ct_name :: String
    , ct_sum :: Bool -- True for sumtypes. Only needed for serializing.
    , ct_fields :: [FT] -- only needed for parseing.
    }

data FT
    = Field
    { ft_index :: Int
    , ft_name :: String
    }

data Prim = forall a. Integral a => PrimInt a
          | PrimChar Char
          | PrimString String
          | PrimUnit --- quick solution for empty constructors.

instance Show Prim
instance Data Prim
instance Typeable Prim --- useful for errors.
 

primIsString :: Prim -> Either String String
primIsString (PrimString s) = Right s
primIsString _ = Left "Primitive is not of string type."

primIsInt :: Num a => Prim -> Either String a
primIsInt (PrimInt i) = Right $ fromIntegral i
primIsInt _ = Left "Primitive is not of integral type."

primString :: Parser (Prim -> Maybe String)
primString = pure $ \p ->
                 case p of
                   PrimString s -> Just s
                   _            -> Nothing

primInt :: Num a => Parser (Prim -> Maybe a)
primInt = pure $ \p -> 
              case p of
                PrimInt i -> Just $ fromIntegral i
                _         -> Nothing

-------------------------------------------------------------------------------
-- SmartCopy
-------------------------------------------------------------------------------
{- This causes compiler error?!
get :: (Data a, Format m, SmartCopy a m)
    => m EncodedType -> m a
get v = runParser (unPack $ parse False False (unPack v)) fail return
-}

class (Format m) => SmartCopy a m where
    version :: Version a
    serialize :: a -> m EncodedType
    -- Default: Outer constructor (object), first field, no sumtype,
    -- not multiple fields.
    serialize a = serializeWithIndex a 0 0 False False
    serializeWithIndex :: a -> Int -> Int -> Bool -> Bool ->  m EncodedType 
    ---- First int argument for tracking constructor level.
    ---- Second int argument for tracking field indices.
    ----First bool tells us if datatype is sumtype.
    ---- Second bool tells us if there are multiple fields.
    default serializeWithIndex :: (Generic a, GSmartCopy (Rep a) m)
                               => a -> Int -> Int -> Bool -> Bool ->  m EncodedType
    serializeWithIndex a = gserialize (from a)

    parse :: Data a => Bool -> Bool -> m EncodedType -> m (Parser a)
    -- First bool argument tells us if datatype is sumtype.
    -- Second bool argument tells us if there are multiple selector fields.
    default parse :: (Generic a, GSmartCopy (Rep a) m, Data a)
                  => Bool -> Bool -> m EncodedType -> m (Parser a)
    parse st mf v = fmap (pure . to) (runParser (unPack (gparse st mf v)) fail return)

class Format m => GSmartCopy t m where
    gserialize :: t x -> Int -> Int -> Bool -> Bool -> m EncodedType
    gparse :: Bool -> Bool -> m EncodedType -> m (Parser (t x))

instance Format m => SmartCopy Integer m where
    serializeWithIndex i _ _ _ _ = writeValue (PrimInt i)
    parse _ _ v = case (parseEither' (unPack . parseValue) v) of
                Right (PrimInt a) -> return $ pure $ toInteger a
                _                 -> fail "Failed to parse integer."

instance Format m => SmartCopy Int m where
    serializeWithIndex i _ _ _ _ = writeValue (PrimInt $ toInteger i)
    parse _ _ v = case (parseEither' (unPack . parseValue) v) of
                Right (PrimInt a) -> return $ pure $ fromInteger $ toInteger a
                _                 -> fail "Failed to parse integer."

instance Format m => SmartCopy Char m where
    serializeWithIndex c _ _ _ _ = writeValue (PrimChar c)
    parse _ _ v = case (parseEither' (unPack . parseValue) v) of
                Right (PrimString s) ->
                    case length s of
                      0 -> return $ pure '\0'
                      1 -> return $ pure $ head s
                      _ -> fail "Failed to parse character, found a string instead."
                _ -> fail "Failed to parse character."

instance Format m => SmartCopy String m where
    serializeWithIndex s _ _ _ _ = writeValue (PrimString s)
    parse _ _ v = case (parseEither' (unPack . parseValue) v) of
                Right (PrimString s) -> return $ pure s
                _                    -> fail "Failed to parse string."

instance (Format m, SmartCopy a m) => SmartCopy [a] m where
    serializeWithIndex [] _ _ _ _ = closeRepetition
    serializeWithIndex a@(x:xs) cl fi st mf = let n = length a
                           in openRepetition n (map (\x -> serializeWithIndex x cl fi st mf) xs)
    parse = undefined

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance Format m => GSmartCopy U1 m where
    gserialize _ _ _ _ _ = returnEmpty
    gparse _ _ _ = return $ pure U1

instance (Format m, GSmartCopy a m, Constructor c) => GSmartCopy (C1 c a) m where
    gserialize m1 cl _ st mf  = let cons = Cons cl 0 (conName m1) st []
---- we only need field list and field index for parsing. 
---- this is not pretty, I'm just passing unneeded stuff.
                                    inside = gserialize (unM1 m1) (cl+1) 0 st False
                                in enterCons cons inside
    gparse True mf v    = undefined
    gparse False mf v   = undefined --fmap ((<$>) M1) (getFromCons (Cons 0 0 (conName (undefined :: C1 c a p)) False []) v)
                    {-
                        where dataType = dataTypeOf ((undefined :: m EncodedType -> a) generalCase)
                              generalCase =
                                let index = return 0
                                in if isAlgType dataType
                                      then index >>= \i -> fromConstrM o (indexConstr dataType (i+1))
                                      else fail ("Not algebraic.")
                                          -}
    {-
    gparse o          = let field = Field 0 conName
                            inside = [] in
                        parseField field inside
                        -}
    ------ THIS NEEDS FIXING. We need correct field index and selectors.

instance (Format m, GSmartCopy a m, Datatype d) => GSmartCopy (D1 d a) m where
    gserialize m1 cl fi st mf = gserialize (unM1 m1) cl fi st False

instance (Format m, GSmartCopy a m, Selector s) => GSmartCopy (S1 s a) m where
    gserialize m1 cl fi st mf = let field = (Field fi (selName m1))
                                    inside = gserialize (unM1 m1) (cl+1) 0 st False
                                in enterField field mf inside

instance (GSmartCopy a m, GSmartCopy b m, Format m)
         => GSmartCopy (a :+: b) m where
    gserialize c@(L1 a) cl fi _ mf  = gserialize a cl fi True mf
    gserialize c@(R1 a) cl fi _ mf = gserialize a cl fi True mf
    gparse st mf v          = let c1 = L1 <$> unPack (gparse True mf v)
                                  c2 = R1 <$> unPack (gparse True mf v)
                                  in return (c1 <|> c2)

instance (GSmartCopy a m, GSmartCopy b m, Format m) => GSmartCopy (a :*: b) m where
    gserialize (a :*: b) cl fi st mf = mult (gserialize a cl fi st True)
                                        (gserialize b cl (fi+1) st True)
    gparse st mf v = do f1 <- gparse st True v
                        f2 <- gparse st True v
                        return $ (:*:) <$> f1 <*> f2

instance (Data a, SmartCopy a m) => GSmartCopy (K1 g a) m where
    gserialize (K1 a) cl fi st mf = serializeWithIndex a cl fi st mf
    gparse st mf v        = do pa <- parse st mf v
                               return $ K1 <$> pa


-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

newtype Parser a
    = Parser
    { runParser :: forall f r. Failure f r -> Success a f r -> f r }

type Failure f r = String -> f r
type Success a f r = a -> f r

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    return a = Parser $ \_ ks -> ks a
    fail msg = Parser $ \kf _ -> kf msg

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'

instance Applicative Parser where
    pure = return
    (<*>) = apP

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e 
    = do b <- d
         a <- e
         return (b a)

instance Alternative Parser where
    empty = fail "empty"
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = fail "mzero"
    mplus a b = Parser $ \kf ks ->
            let kf' _ = runParser b kf ks
            in runParser a kf' ks

instance Monoid (Parser a) where
    mempty = fail "mempty"
    mappend = mplus


parseEither :: Format m => (a -> m (Parser b)) -> a -> Either String b
parseEither pm v = runParser (unPack (pm v)) Left Right

parseEither' :: (a -> Parser b) -> a -> Either String b
parseEither' m v = runParser (m v) Left Right

getField :: Format m => Int -> String -> [FT] -> (m EncodedType -> Either String Prim)
getField index name fields = parseEither (parseField (Field index name) fields)

-- Fail if there is a fail in a list of parseEither-Results.
-- Otherwise perform action on all (right) results.
eitherList :: ([b] -> c) -> [Either a b] -> [b] -> Either a c
eitherList right [] acc= Right $ right acc
eitherList right list@(e:es) acc
    = case e of
        Right r -> eitherList right es (acc++[r])
        Left l -> Left l


----
-- Operators for error handling of primvalue-conversion
----

(<.$>) :: (a -> b) -> Parser (Maybe a) -> Parser b
cons <.$> p = do res <- runParser p fail return
                 case res of
                   Just a -> pure $ cons a
                   Nothing -> empty

(<.*>) :: Parser (a -> b) -> Parser (Maybe a) -> Parser b
pa <.*> pb = do res1 <- runParser pa fail return
                res2 <- pb
                case res2 of
                  Just a -> pure $ res1 a
                  Nothing -> empty

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

