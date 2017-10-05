module FreeMock (demos) where

import Test.Tasty (testGroup, TestTree)

demos :: TestTree
demos = testGroup "Free Mock" []
