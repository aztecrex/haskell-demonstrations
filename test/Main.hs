
module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified TypeclassMock
import qualified FreeMock
import qualified BigStack

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [
        TypeclassMock.demos,
        FreeMock.demos,
        BigStack.demos
    ]
