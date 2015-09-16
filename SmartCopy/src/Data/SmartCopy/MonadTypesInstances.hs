{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- |The Fail and FailT monad used in the SmartCopy parse formats.
module Data.SmartCopy.MonadTypesInstances where

------------------------------------------------------------------------------
-- LOCAL
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- SITE-PACKAGES
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- STDLIB
------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.IO.Class

import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import "mtl" Control.Monad.State

-- |An error handling monad.
newtype FailT m a = FailT { runFailT :: m (Fail a) }

-- |Represents values that can be either an error or correct, where the Fail
-- constructor is used to hold an error message and the Ok constructor is
-- used to hold a correct value of type a.
data Fail a
    = Fail String
    | Ok a deriving Show

-- |Takes a Fail and if it holds a correct value returns the value in a monad,
-- or if it holds an error fails in the monad.
fromOk :: Monad m => Fail a -> m a
fromOk (Ok a) = return a
fromOk (Fail a) = fail a

-- |Converts an Either value into a Fail value.
fromEitherFail :: Either String a ->  Fail a
fromEitherFail (Left msg) = Fail msg
fromEitherFail (Right ok) = Ok ok

instance Functor Fail where
    fmap f (Ok a) = Ok $ f a
    fmap _ (Fail msg) = Fail msg

instance Applicative Fail where
    pure = Ok
    f <*> (Ok b) = do f' <- f
                      pure $ f' b
    _ <*> (Fail a) = Fail a

instance (Applicative m, Monad m) => Applicative (FailT m) where
    pure = FailT . pure . Ok
    f <*> m = FailT $ do f' <- runFailT f
                         m' <- runFailT m
                         return $ f' <*> m'

instance Alternative Fail where
    empty = Fail "mzero"
    (<|>) a@(Ok _) _ = a
    (<|>) _ a = a

instance Functor m => Functor (FailT m) where
    fmap f = FailT . fmap (fmap f) . runFailT

instance MonadReader r m => MonadReader r (FailT m) where
    local f (FailT m) =
        FailT (local f m)
    ask = FailT (liftM Ok ask)

instance MonadState r m => MonadState r (FailT m) where
    get = FailT (liftM Ok get)
    put s = FailT (liftM Ok (put s))

instance MonadIO m => MonadIO (FailT m) where
    liftIO a = FailT (liftM Ok (liftIO a))

instance Monad m => Monad (FailT m) where
    return a = FailT $ return $ Ok a
    (FailT ma) >>= fb =
        FailT $
        do a <- ma
           case a of
             Ok b -> runFailT $ fb b
             Fail b -> fail b

instance MonadTrans FailT where
    lift ma =
        FailT $ do a <- ma
                   return $ Ok a

instance (Applicative m, Monad m) => Alternative (FailT m) where
    empty = FailT (return $ Fail "mzero")
    ma <|> mb = FailT $
                do a <- runFailT ma
                   case a of
                     ok@(Ok _) -> return ok
                     _ -> runFailT mb

instance Monad Fail where
    return = Ok
    fail = Fail
    fa >>= fb =
        case fa of
          Ok a -> fb a
          Fail a -> Fail a
