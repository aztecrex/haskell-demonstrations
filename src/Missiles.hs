module Missiles (Missiles, launch) where

import Control.Monad.Trans.Writer (Writer, tell)

class Monad m => Missiles m where
    launch :: m ()

