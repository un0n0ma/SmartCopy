{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Instances where

import Control.Applicative
import "mtl" Control.Monad.Identity
import "mtl" Control.Monad.Reader
import Control.Monad.IO.Class
import "mtl" Control.Monad.Writer
import "mtl" Control.Monad.Trans (MonadTrans(..))

newtype FailT m a = FailT { runFailT :: m (Fail a) }

data Fail a = Fail String
            | Ok a deriving Show

fromOk :: Monad m => Fail a -> m a
fromOk (Ok a) = return a
fromOk (Fail a) = fail a

instance Functor Fail where
    fmap f (Ok a) = Ok $ f a
    fmap f (Fail msg) = Fail msg

instance Applicative Fail where
    pure = Ok
    f <*> (Ok b) = do f' <- f
                      pure $ f' b
    f <*> (Fail a) = Fail a

instance (Applicative m, Monad m) => Applicative (FailT m) where
    pure = FailT . pure . Ok
    f <*> m = FailT $ do f' <- runFailT f
                         m' <- runFailT m
                         return $ f' <*> m'

instance Functor m => Functor (FailT m) where
    fmap f = FailT . (fmap (fmap f)) . runFailT

instance MonadReader r m => MonadReader r (FailT m) where
    local f (FailT m) =
        FailT (local f m)
    ask = FailT (ask >>= return . Ok)

instance MonadIO m => MonadIO (FailT m) where
    liftIO a = FailT (liftIO a >>= (return . Ok))

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

instance Monad Fail where
    return = Ok
    fail = Fail
    fa >>= fb =
        case fa of
          Ok a -> fb a
          Fail a -> Fail a
