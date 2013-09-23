-- This is an example of what we *don't* want.  We use, e.g., IVars as
-- a mechanism to avoid races like this.

import Control.Concurrent

p = do
  num <- newEmptyMVar
  forkIO $ do putMVar num 3
  forkIO $ do putMVar num 4
  v <- takeMVar num
  return v

main = do
  v <- p
  putStr $ show v -- nondeterministic -- could be either 3 or 4.  (In
                  -- practice, I seem to almost always get 4, with a
                  -- stray 3 now and then.)
