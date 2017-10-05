{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (execWriter, Writer, tell)
import Control.Monad.Trans.State

import Missiles (Missiles, launch)

type WriterMock e = Writer [e]
type WriteStringMock = WriterMock String

type StateMock s = State s
type StateBoolMock = StateMock Bool


demos = testGroup "Typeclass Mock" [
        testCase "Simple Writer" $
            execWriter backdoor @?= ["launched"]
       , testCase "Simple State" $
            execState backdoor False  @?= True
    ]

instance Missiles WriteStringMock where
    launch = tell ["launched"]

instance Missiles StateBoolMock where
    launch = put True

backdoor :: Missiles m => m ()
backdoor = launch
