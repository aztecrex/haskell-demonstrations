{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (execWriter, Writer, tell)
import Control.Monad.Trans.State

import Missiles (Missiles, launch)

demos = testGroup "Typeclass Mock" [
        testCase "Writer" $
            execWriter backdoor @?= ["launched"]
       , testCase "State" $
            execState backdoor False  @?= True
    ]

-- a logging mock
instance Missiles (Writer [String]) where
    launch = tell ["launched"]

-- a transformation mock
instance Missiles (State Bool) where
    launch = put True

-- The function under test
backdoor :: Missiles m => m ()
backdoor = launch
