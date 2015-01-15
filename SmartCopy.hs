{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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
import Data.Data



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

class (Monoid (m EncodedType), Functor m, Monad m) => Format m where
    type EncodedType :: *
    type EncodedTypeVersioned :: *
    unPack :: m a -> a
    returnEmpty :: m EncodedType

------ Encoding
    enterCons :: CT -> m EncodedType -> m EncodedType
    enterField :: FT -> m EncodedType -> m EncodedType
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
    ---- getFromCons & parseField need different return types.
    getFromCons :: CT -> m EncodedType -> m (Parser Prim)
    parseField :: FT -> [FT] -> m EncodedType -> m (Parser Prim)
   -- parseRepetition :: m EncodedType -> m (Parser a)
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

primString :: Parser (Prim -> String)
primString = pure $ \p ->
                 case p of
                   PrimString s -> s
                 -- prim           -> return $ fail ("Expected string primitive. Got: " ++ show prim)

primInt :: (Num a) => Parser (Prim -> a)
primInt = pure $ \p -> 
              case p of
                PrimInt i -> fromIntegral i
          --      prim      -> fail ("Expected number primitive. Got: " ++ show prim)

-------------------------------------------------------------------------------
-- SmartCopy
-------------------------------------------------------------------------------
class (Format m) => SmartCopy a m where
    version :: Version a
    serialize :: a -> m EncodedType
    serialize a = serializeWithIndex a 0 False
    serializeWithIndex :: a -> Int -> Bool ->  m EncodedType ---- Int for tracking field indices
    default serializeWithIndex :: (Generic a, GSmartCopy (Rep a) m)
                               => a -> Int -> Bool ->  m EncodedType
    serializeWithIndex a = gserialize (from a)
    parse :: m EncodedType -> m a
    default parse :: (Generic a, GSmartCopy (Rep a) m)
                  => m EncodedType -> m a
    parse b = fmap to (gparse b)

class Format m => GSmartCopy t m where
    gserialize :: t x -> Int -> Bool -> m EncodedType
    gparse :: m EncodedType -> m (t x)

instance Format m => SmartCopy Integer m where
    serializeWithIndex i _ _ = writeValue (PrimInt i)
    parse b = case (parseEither' (unPack . parseValue) b) of
                Right (PrimInt a) -> return $ toInteger a
                _ -> fail "Failed to parse integer."

instance Format m => SmartCopy Int m where
    serializeWithIndex i _ _ = writeValue (PrimInt $ toInteger i)
    parse b = undefined

instance Format m => SmartCopy Char m where
    serializeWithIndex c _ _ = writeValue (PrimChar c)
    parse b = undefined

instance Format m => SmartCopy String m where
    serializeWithIndex s _ _ = writeValue (PrimString s)
    parse b = undefined

instance (Format m, SmartCopy a m) => SmartCopy [a] m where
    serializeWithIndex [] _ _ = closeRepetition
    serializeWithIndex a@(x:xs) i b = let n = length a
                           in openRepetition n (map (\x -> serializeWithIndex x i b) xs)

-------------------------------------------------------------------------------
-- GenericInstances
-------------------------------------------------------------------------------

instance Format m => GSmartCopy U1 m where
    gserialize _ _ _ = returnEmpty

instance (Format m, GSmartCopy a m, Constructor c) => GSmartCopy (C1 c a) m where
    gserialize m1 _ b = let cons = Cons 0 (conName m1) b []
---- we only need field list and field index for parsing. 
---- this is not pretty, I'm just passing unneeded stuff.
                            inside = gserialize (unM1 m1) 0 b
                        in enterCons cons inside

instance (Format m, GSmartCopy a m, Datatype d) => GSmartCopy (D1 d a) m where
    gserialize m1 i b = gserialize (unM1 m1) i b

instance (Format m, GSmartCopy a m, Selector s) => GSmartCopy (S1 s a) m where
    gserialize m1 i b = let field = (Field i (selName m1))
                            inside = gserialize (unM1 m1) 0 b
                        in enterField field inside

instance (GSmartCopy a m, GSmartCopy b m, Format m) => GSmartCopy (a :+: b) m where
    gserialize c@(L1 a) i _ = gserialize a i True
    gserialize c@(R1 a) i _ = gserialize a i True

instance (GSmartCopy a m, GSmartCopy b m, Format m) => GSmartCopy (a :*: b) m where
    gserialize (a :*: b) i c = mult (gserialize a i c) (gserialize b (i+1) c)

instance SmartCopy a m => GSmartCopy (K1 g a) m where
    gserialize (K1 a) i b = serializeWithIndex a i b


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
    (<|>) = mplus --------- FIX THIS. NEEDED FOR SUMTYPES.

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

-------------------------------------------------------------------------------
-- Other
-------------------------------------------------------------------------------

