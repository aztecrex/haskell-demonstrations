{-# LANGUAGE FlexibleInstances #-}

module TypeclassMock (demos) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.Writer (execWriter, Writer, tell)
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

import Missiles (Missiles, launch)

demos :: TestTree
demos = testGroup "Typeclass Mock" [
        testCase "Backdoor with writer" $
            execWriter backdoor @?= ["launched"]
      , testCase "Backdoor with state" $
            execState backdoor False  @?= True
      , testCase "Frontdoor succeed" $
            (execWriter $ execStateT (enter "pa$$word123" >> fire) "") @?= ["launched"]
      , testCase "Frontdoor fail" $
            (execWriter $ execStateT (enter "h4x0r" >> fire) "") @?= ([] :: [String])
    ]

-- a logging mock
instance Missiles (Writer [String]) where
    launch "pa$$word123" = tell ["launched"]
    launch _ = pure ()

-- a transformation mock
instance Missiles (State Bool) where
    launch "pa$$word123" = put True
    launch _ = pure ()

-- backdoor function under test
backdoor :: Missiles m => m ()
backdoor = launch "pa$$word123"


class Monad m => Panel m where
    enter :: String -> m ()
    fire :: m ()

-- panel implementation under test
type PanelState = String
instance Missiles m => Panel (StateT PanelState m) where
    enter = put
    fire = do
        code <- get
        lift $ launch code

