module FreeMock (demos) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Free

import Robot

demos :: TestTree
demos = testGroup "Free Mock" [
     testCase "panic flees and fires 3 times" $
       runMock panic @?= (1.0, 3)
  ]

-- the mock
runMock :: Op r -> (Double, Int)
runMock (Pure r) = (0.0, 0)
runMock (Free (Fire op)) = bump $ runMock op
  where bump (x,y) = (x, y + 1)
runMock (Free (Throttle val op)) = set val $ runMock op
  where set v (_, y) = (v, y)
runMock (Free (Status sys fop)) = runMock (fop True)

-- function under test
panic :: Op ()
panic = do
    throttle' 1.0
    fire'
    fire'
    fire'

-- setting up the effectful operations as operations in Free
data OpData x = Fire x
              | Throttle Double x
              | Status SubSystem (Bool -> x)

-- this can probably be derived, also there are "free-er" monads that might not require this
instance Functor OpData where
    fmap f (Fire x) = Fire (f x)
    fmap f (Throttle v x) = Throttle v (f x)
    fmap f (Status sys k) = Status sys (f . k)

type Op = Free OpData

fire' :: Op ()
fire' = liftF $ Fire ()

throttle' :: Double -> Op ()
throttle' v = liftF $ Throttle v ()

status' :: SubSystem -> Op Bool
status' sys = liftF $ Status sys id



