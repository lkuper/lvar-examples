-- A version of set-ioref-data-race.hs with enough synchronization to
-- be deterministic, but we had to manually add the calls to `wait` in
-- the right places.

import Control.Concurrent.Async
import Control.Concurrent
import Data.IORef
import Data.Set

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: IO (Set Item)
p = do cart <- newIORef empty
       a1 <- async $ atomicModifyIORef cart (\s -> (insert Book s, ()))
       a2 <- async $ atomicModifyIORef cart (\s -> (insert Shoes s, ()))
       res <- async $ do waitBoth a1 a2
                         readIORef cart
       wait res

main = do v <- p
          print $ toList v
