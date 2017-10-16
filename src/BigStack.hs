module BigStack where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Functor.Identity

demos :: TestTree
demos = testGroup "Big Stack" [
    testCase "thread the execs" $ do
        let values = ("a", "b", "c")
        execStack (initStack values) @?= values,
    testCase "composedExec" $ do
        let values = ("a", "b", "c")
        execStack' (initStack values) @?= execStack (initStack values),
    testCase "init, rotate, get, eval" $
        evalStack (initStack ("a","b","c") >> rotateStack >> getStack)
          @?= ("b", "c", "a")
  ]

type BigStack = StateT String (StateT String (StateT String Identity))

execStack :: BigStack a -> (String, String, String)
execStack stack = (a, b, c)
      where a = runIdentity $ evalStateT (evalStateT (execStateT stack def) def) def
            b = runIdentity $ evalStateT (execStateT (runStateT stack def) def) def
            c = runIdentity $ execStateT (runStateT (runStateT stack def) def) def
            def = ""

execStack' :: BigStack a -> (String, String, String)
execStack' s = evalStack $ s >> getStack

getStack :: BigStack (String, String, String)
getStack = do
    a <- get
    b <- lift $ get
    c <- lift $ lift $ get
    return (a, b, c)

evalStack :: BigStack a -> a
evalStack stack = runIdentity (evalStateT (evalStateT (evalStateT stack def) def) def)
    where def = ""


rotateStack :: BigStack ()
rotateStack = do
    a <- get
    b <- lift $ get
    c <- lift $ lift $ get
    put b
    lift $ put c
    lift $ lift $ put a

initStack :: (String, String, String) -> BigStack ()
initStack (a, b, c) = do
    put a
    lift $ put b
    lift $ lift $ put c


pass :: Monad m => StateT String (StateT String m) ()
pass = do
  v <- get
  lift $ put (v ++ v)

emit :: StateT String Identity Int
emit = do
  v <- get
  lift $ pure $ length v

-- process :: String -> BigStack Int
-- process =
-- runStack :: BigStack a -> String
-- runStack stack = runIdentity $ execStateT stack "whatever"


-- unpack :: BigStack () -> String
-- unpack = runIdentity $ do
--   s <- get
--   return s


  -- f :: String -> String
-- f s = runIdentity $ execState $ do
--   put s
--   get
