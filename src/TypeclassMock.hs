{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (Writer, tell)
import Missiles (Missiles, launch)

type WriterMock = Writer [String]

demos = testGroup "Typeclass Mock" [
        testCase "Simple Writer" $
            (backdoor :: WriterMock ()) @?= tell ["launched"]
    ]

instance Missiles WriterMock where
    launch = tell ["launched"]

backdoor = launch
