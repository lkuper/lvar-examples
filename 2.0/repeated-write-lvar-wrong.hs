-- This program attempts to do two conflicting writes to an LVar and
-- deterministically raises an error.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.IVar -- The particular lattice in question.

p :: (HasPut e, HasGet e) => Par e s Int
p = do
  num <- new
  fork (put num 3)
  fork (put num 4)
  get num

main = do
  putStr $ show $ runPar $ p
