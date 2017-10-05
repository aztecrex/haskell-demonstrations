{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (Writer, tell)
import Missiles (Missiles, launch)

type WriterMock e = Writer [e]
type WriteStringMock = WriterMock String

demos = testGroup "Typeclass Mock" [
        testCase "Simple Writer" $
            (backdoor :: WriteStringMock ()) @?= tell ["launched"]
    ]

instance Missiles WriteStringMock where
    launch = tell ["launched"]

backdoor = launch
