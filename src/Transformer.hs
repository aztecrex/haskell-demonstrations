{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Transformer (demos) where

import Test.Tasty (TestTree, testGroup)
import Data.Default
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import System.Environment

demos :: TestTree
demos = testGroup "Transformer" [

  ]

newtype ParametersT e o m a = ParametersT {
    runParametersT :: Either e o -> m (Either e a)
  }

options :: (Applicative m) => ParametersT e o m o
options = ParametersT $ \eo -> case eo of
    Left err -> pure $ Left err
    Right o -> pure $ Right o


instance (Functor m) => Functor (ParametersT e o m) where
    fmap f (ParametersT fa) = ParametersT $
        \r -> (fmap . fmap) f (fa r)

instance (Applicative m) => Applicative (ParametersT e o m) where
    pure = liftParametersT  . pure
    ParametersT mf <*> ParametersT mx = ParametersT $ \eo -> liftA2 (<*>) (mf eo) (mx eo)

liftParametersT :: (Functor m) => m a -> ParametersT e o m a
liftParametersT mx = ParametersT $ \eo -> fmap (guard eo) mx
    where guard (Left err) _ = Left err
          guard (Right _) x = Right x

instance (Monad m) => Monad (ParametersT e o m) where
    ma >>= f = ParametersT $ \eo -> do
       ea <- runParametersT ma eo
       case ea of
        Right a -> runParametersT (f a) eo
        Left err -> pure $ Left err

instance MonadTrans (ParametersT e o) where
    lift = liftParametersT

instance (MonadIO m) => MonadIO (ParametersT e o m) where
    liftIO = lift . liftIO

type Parse e o = [String] -> Either e o

withArgs :: (MonadIO m) => ParametersT e o m [String]
withArgs = do
    args <- liftIO getArgs
    pure args



-- arguments :: (MonadIO m) => Parse e o -> m a -> ParametersT e o m a
-- arguments parse program = do
--   args <- (lift . liftIO) getArgs
