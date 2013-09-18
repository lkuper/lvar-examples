-- Because we write the same thing twice, this program is
-- deterministic, but it's nevertheless ruled out by the IVar
-- single-put restriction.  It will raise a "multiple put" error.

import Control.Monad.Par

f :: Par Int
f = do
  i <- new
  fork $ do put i 3
  fork $ do put i 3
  v <- get i
  return v

main = do
  putStr $ show $ runPar $ f
