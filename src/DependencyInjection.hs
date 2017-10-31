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
            let size = 17
                program = barage :: Stub ()                  -- inject with stub and mock
                actual = execWriterT program size            -- when injected program is run
            in actual @?= take size (repeat "launchc0de")    -- then ...
    ]

-- code with side-effectful dependencies
-- barage picks a random number and launches that many
-- missiles
barage :: (Missiles m, Rand m) => m ()
barage = do
    size <- fromRange 12 25
    replicateM_ size (launch "launchc0de")

-- an interface to a module that probably has side effects
class (Monad m) => Rand m where
    fromRange :: Int -> Int -> m Int

-- a stub implementation of Rand
-- simply returns whatever number it is given
instance Rand ((->) Int) where
    fromRange lo hi = (clamp lo hi) . id where
        clamp lo' hi' x = min hi' (max lo' x)

-- a mock implementation of Missiles
-- keep track of all the launches
instance (Monad m) => Missiles (WriterT [String] m) where
    launch p = tell [p]

-- custom mock + stub for missiles and random number
type Stub = WriterT [String] ((->) Int)

instance Rand Stub where
    fromRange l h = lift $ fromRange l h
