-- Ruling out nondeterminism with IVars.  This will run and
-- deterministically raise a "multiple put" error, because we tried to
-- write to `num` twice.

import Control.Monad.Par

p :: Par Int
p = do
  num <- new
  fork $ do put num 3
  fork $ do put num 4
  v <- get num
  return v

main = do
  putStr $ show $ runPar $ p
