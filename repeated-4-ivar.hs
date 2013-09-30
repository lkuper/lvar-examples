-- Because we write the same thing twice, this program is
-- deterministic, but it's nevertheless ruled out by the IVar
-- single-put restriction.  It will raise a "multiple put" error.

import Control.Monad.Par

p :: Par Int
p = do
  num <- new
  fork $ put num 4
  fork $ put num 4
  get num

main = do
  putStr $ show $ runPar $ p
