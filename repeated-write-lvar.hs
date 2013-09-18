-- This program writes the same thing twice and deterministically
-- prints `3` instead of raising an error.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.IVar -- The particular lattice in question.

f :: Par Det s Int
f = do
  i <- new
  fork $ do put i 3
  fork $ do put i 3
  v <- get i
  return v

main = do
  putStrLn $ show $ runPar $ f
