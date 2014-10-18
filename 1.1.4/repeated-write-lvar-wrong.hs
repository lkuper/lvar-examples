-- This program attempts to do two conflicting writes to an LVar and
-- deterministically raises an error.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.IVar -- The particular lattice in question.

p :: Par Det s Int
p = do
  num <- new
  fork (put num 3)
  fork (put num 4)
  get num

main = print $ runPar p
