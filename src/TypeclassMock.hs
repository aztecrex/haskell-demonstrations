{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (execWriter, Writer, tell)
import Control.Monad.Trans.State

import Missiles (Missiles, launch)

demos = testGroup "Typeclass Mock" [
        testCase "Backdoor with writer" $
            execWriter backdoor @?= ["launched"]
      , testCase "Backdoor with state" $
            execState backdoor False  @?= True
    ]

-- a logging mock
instance Missiles (Writer [String]) where
    launch "pa$$word123" = tell ["launched"]
    launch _ = pure ()

-- a transformation mock
instance Missiles (State Bool) where
    launch "pa$$word123" = put True
    launch _ = pure ()

-- The function under test
backdoor :: Missiles m => m ()
backdoor = launch "pa$$word123"

