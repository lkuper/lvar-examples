-- This program attempts to do two conflicting writes to an LVar and
-- deterministically raises an error.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.IVar -- The particular lattice in question.

f :: Par Det s Int
f = do
  i <- new
  fork $ do put i 3
  fork $ do put i 4
  v <- get i
  return v

main = do
  putStr $ show $ runPar $ f
