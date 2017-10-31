{-# LANGUAGE FlexibleInstances #-}
module DependencyInjection (demos) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad (replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (WriterT, tell, execWriterT)
import Missiles (Missiles(..))



demos :: TestTree
demos = testGroup "DependencyInjection" [
        testCase "here be stubs" $
            let program = barage :: Stub ()                -- given injected
                actual = (execWriterT program) 17          -- when run with injected
            in actual @?= take 17 (repeat "launchc0de")    -- then ...
    ]

class (Monad m) => Rand m where
    fromRange :: Int -> Int -> m Int

barage :: (Missiles m, Rand m) => m ()
barage = do
    size <- fromRange 12 25
    replicateM_ size (launch "launchc0de")


-- a stub implementation of Rand
instance Rand ((->) Int) where
    fromRange lo hi = (clamp lo hi) . id where
        clamp lo' hi' x = min hi' (max lo' x)

-- a stub implementation of Missiles
instance (Monad m) => Missiles (WriterT [String] m) where
    launch p = tell [p] >> pure ()

-- custom stub for both
type Stub = WriterT [String] ((->) Int)
instance Rand Stub where
    fromRange l h = lift $ fromRange l h

