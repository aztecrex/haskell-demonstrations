
module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified TypeclassMock

main :: IO ()
main = defaultMain tests

tests = testGroup "All" [
        TypeclassMock.demos
    ]
