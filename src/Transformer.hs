{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Transformer (demos) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Data.Default
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import System.Environment

demos :: TestTree
demos = testGroup "Transformer" [
    testCase "success" $
        runReader thing ["one", "two", "three"] @?= "result: 9",
    testCase "failure" $
        runReader thing ["nonsense"] @?= "error: bad arguments",
    testCase "success without a parser" $
        runIdentity (thing2 (Right 4)) @?= "result: 16",
    testCase "success without a parser" $
        runIdentity (thing2 (Left "whoops!")) @?= "error: whoops!"
  ]

-- The type for the parameters Monad. Its type parameters
-- are o: tye type of options available to a program
--     e: an error discovered in obtaining options
--     m: the embedded monad
--     a: the encapsulated value
--
-- the result of runParametersT accepts a successful or
-- failed options creation result
newtype ParametersT e o m a = ParametersT {
    runParametersT :: Either e o -> m (Either e a)
  }

-- How to get options from the monad. Makes the options
-- value the encapsulated value or fails
options :: (Applicative m) => ParametersT e o m o
options = ParametersT $ \eo -> case eo of
    Left err -> pure $ Left err
    Right o -> pure $ Right o

-- Functor needs to fmap the fmap because of the Either inside
instance (Functor m) => Functor (ParametersT e o m) where
    fmap f (ParametersT fa) = ParametersT $
        \eo -> (fmap . fmap) f (fa eo)

-- Applicative needs to liftA2 the application because of the Either inside
instance (Applicative m) => Applicative (ParametersT e o m) where
    pure = liftParametersT  . pure
    ParametersT mf <*> ParametersT mx = ParametersT $ \eo -> liftA2 (<*>) (mf eo) (mx eo)

-- How ParametersT composes with other Monads
liftParametersT :: (Functor m) => m a -> ParametersT e o m a
liftParametersT mx = ParametersT $ \eo -> fmap (guard eo) mx
    where guard (Left err) _ = Left err
          guard (Right _) x = Right x

-- Monad is pretty easy because ParametersT can count on the rest of the
-- stack to be a monad, just extract the Either options and see if
-- they are Right. If so, continue, if not, don't
instance (Monad m) => Monad (ParametersT e o m) where
    ma >>= f = ParametersT $ \eo -> do
       ea <- runParametersT ma eo
       case ea of
        Right a -> runParametersT (f a) eo
        Left err -> pure $ Left err

-- Convenience, lift a computation from the embedded Monad
instance MonadTrans (ParametersT e o) where
    lift = liftParametersT

-- Convenience, lift IO from the embedded Monad
instance (MonadIO m) => MonadIO (ParametersT e o m) where
    liftIO = lift . liftIO

-- What a parser looks like if it's going to be used with
-- 'parameters'
type ParseArgs e o = [String] -> Either e o

-- Nice way to designate that an embedded Monad can get some
-- arguments to parse
class (Monad m) => HasArgs m where
    getArgs' :: m [String]

-- For the demo, a reader just gives up its args. This is
-- how ParametersT can be tested
instance HasArgs (Reader [String]) where
    getArgs' = ask

-- Here's how you can use IO or something that can liftIO. Don't
-- do an undecidable instance on liftIO or it will match too many
-- things (e.g. the Reader [String] instance for the demo) and you
-- won't be able to test it outside of IO
instance HasArgs IO where
    getArgs' = getArgs

-- How you use the monad from within another that supplies arguments
parameters :: (HasArgs m) => ParseArgs e o -> ParametersT e o m a -> m (Either e a)
parameters fp stuff = do
    args <- getArgs'
    runParametersT stuff (fp args)

parameters' :: Either e o -> ParametersT e o m a -> m (Either e a)
parameters' eitherOptions stuff = do
    runParametersT stuff eitherOptions

parse' :: [String] -> Either String Int
parse' ["nonsense"] = Left "bad arguments"
parse' args = Right $ length args

thing :: Reader [String] String
thing = do
    result <- parameters parse' $ do
        opts <- options
        let result = opts * opts
        pure result
    pure $ either (("error: " ++) <$> id) (("result: " ++) <$> show) result

thing2 :: Either String Int -> Identity String
thing2 eo = do
    result <- parameters' eo $ do
        opts <- options
        let result = opts * opts
        pure result
    pure $ either (("error: " ++) <$> id) (("result: " ++) <$> show) result
