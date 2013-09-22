-- Warning: I don't work yet!

-- This program does two writes to an LVar that are resolved with
-- `max`.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.MaxCounter -- The particular lattice in question.

p :: Par Det s Int
p = do
  num <- newMaxCounter 0
  fork $ do put num 3
  fork $ do put num 4
  waitThresh num 4

main = do
  putStr $ show $ runPar $ p
