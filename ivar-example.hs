-- Ruling out nondeterminism with IVars.  This will run and
-- deterministically raise a "multiple put" error, because we tried to
-- write to i twice.

import Control.Monad.Par

p :: Par Int
p = do
  i <- new
  fork $ do put i 3
  fork $ do put i 4
  v <- get i
  return v

main = do
  putStr $ show $ runPar $ p
