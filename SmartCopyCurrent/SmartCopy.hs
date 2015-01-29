{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}


module SmartCopy where

-------------------------------------------------------------------------------
-- LOCAL
-------------------------------------------------------------------------------
import MonadTypesInstances

-------------------------------------------------------------------------------
-- SITE-PACKAGES
-------------------------------------------------------------------------------
import Data.Text.Internal as T

-------------------------------------------------------------------------------
-- STDLIB
-------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.Trans (MonadTrans(..))
import "mtl" Control.Monad.Writer


class SmartCopy a where
--    version :: Version a
    writeSmart :: (SmartCopy a, Monad m) => SerializationFormat m r -> a -> m ()
    readSmart :: (Functor m, Applicative m, Monad m) => ParseFormat i m -> m a


serializeSmart :: (Monad m, SmartCopy a)
               => SerializationFormat m r -> a -> r
serializeSmart fmt a = (runSerialization fmt) (writeSmart fmt a)

parseSmart :: (Functor m, Monad m, Applicative m, SmartCopy a)
           => ParseFormat i m -> i -> Fail a
parseSmart fmt = (runParser fmt) (readSmart fmt)


instance SmartCopy Int where
    readSmart fmt =
        fmap fromIntegral $ readNum fmt
    writeSmart fmt i =
        writePrimitive fmt $ PrimInt i
        
data SerializationFormat m r -- was ist das r bzw. warum steht das an dieser Stelle? Du willst doch nicht
-- für jeden möglichen Rückgabetyp ein eigenes ParseFormat definieren.
    = SerializationFormat
    { runSerialization :: m () -> r
    , beginWritingCons :: Cons -> m () -- wie schon besprochen: besser "writeCons :: Cons -> m () -> m ()"
    , withField :: Either Int LabeledField -> m () -> m () -- wie unten beim ParseFormat: du kannst dir überlegen
    -- ob du den "Either Int LabeledField" Parameter weglässt.
    , writePrimitive :: Prim -> m ()
    , endWritingCons :: m ()
    -- schreiben von Primitivwerten fehlt noch
    }

data ParseFormat i m -- Was ist das i?
    = ParseFormat
    { runParser :: SmartCopy a => m a -> i -> Fail a
    -- warum brauchst du eigentlich überall die SmartCopy Constraints?? Ich vermute, die sind unnötig (hab's
    -- aber nicht ausprobiert)
    , readCustom :: SmartCopy a => [(Cons, m a)] -> m a -- ich würde readCustom umbennenn nach parseCons
    , readField :: SmartCopy a => Either Int LabeledField -> m a -> m a  -- readField -> parseField
    -- ich würde anstatt "Either Int LabeledField" einen eigenen Datentyp einführen
    -- was du noch bedenken solltest: die Information über die Felder muss der Benutzer des Formats
    -- jetzt doppelt mitgeben: einmal in Cons und einmal über "Either Int LabeledField". Das ist nicht so schön.
    -- Du kannst dir überlegen, ob du readField nicht die einfachere Signature "readField ::  m a -> m a"
    -- geben willst. Dann müssen die readField Aufruf halt in der richtige Reihenfolge erfolgen.
    -- Aber: wenn du einen streaming Parser haben willst muss das sowieso so sein. Was ich damit meine: wenn
    -- du beim Serialisieren erste das Feld foo und dann das Feld bar schreibst, und du willst dann Parsen
    -- ohne komplett den serialisierten ByteString zu lesen, dann klappt das nicht, wenn du zuerst readField für
    -- bar aufrufst.
    , readNum :: m Int  -- --> parseNum
    , readBool :: m Bool  -- --> parseBool
    -- warum nicht einfach readPrim?
    }



-------------------------------------------------------------------------------
--Other
-------------------------------------------------------------------------------

data Cons
    = C
    { cname :: T.Text
    , cfields :: Either Int [LabeledField]
    , ctagged :: Bool
    , cindex :: Int
    }

type LabeledField = T.Text

data Prim = PrimInt Int
          | PrimString String
          | PrimBool Bool


