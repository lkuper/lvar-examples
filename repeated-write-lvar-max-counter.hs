-- Warning: I don't work yet!

-- This program does two writes to an LVar that are resolved with
-- `max`.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.MaxCounter -- The particular lattice in question.

p :: Par Det s ()
p = do
  num <- newMaxCounter 0
  fork $ put num 3
  fork $ put num 4
  waitThresh num 4

main = do
  putStr $ show $ runPar $ p
