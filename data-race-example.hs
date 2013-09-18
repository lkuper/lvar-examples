-- This is an example of what we *don't* want.  We use, e.g., IVars as
-- a mechanism to avoid races like this.

import Control.Concurrent

main = do
  i <- newEmptyMVar
  forkIO $ do putMVar i 3
  forkIO $ do putMVar i 4
  v <- takeMVar i
  putStrLn $ show v -- nondeterministic -- could be either 3 or 4.
                    -- (In practice, I seem to almost always get 4,
                    -- with a stray 3 now and then.)



  