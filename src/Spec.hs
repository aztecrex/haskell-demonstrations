
module Spec where

import Test.Tasty (defaultMain, testGroup)

import qualified TypeclassMock

testMain :: IO ()
testMain = defaultMain tests

tests = testGroup "All" [
        TypeclassMock.demos
    ]
