-- Ruling out nondeterminism with IVars.  This will run and
-- deterministically raise a "multiple put" error, because we tried to
-- write to `num` twice.

import Control.Monad.Par

p :: Par Int
p = do
  num <- new
  fork (put num 3)
  fork (put num 4)
  get num

main = do
  putStr $ show $ runPar $ p
