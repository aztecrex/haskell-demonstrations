
module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)

import qualified TypeclassMock
import qualified FreeMock

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All" [
        TypeclassMock.demos,
        FreeMock.demos
    ]
