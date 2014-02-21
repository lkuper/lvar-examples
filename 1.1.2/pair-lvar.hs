-- Warning: I don't work yet!  Data.LVar.Pair isn't exposed.

{-# LANGUAGE DataKinds #-}

import Control.LVish  -- Generic scheduler; works with any lattice.
import Data.LVar.Pair -- The particular lattice in question.

p :: Par Det s Int
p = do
  nn <- newPair
  fork (putFst nn 0)
  fork (putSnd nn 1)
  getSnd nn

main = do
  print $ runPar p
