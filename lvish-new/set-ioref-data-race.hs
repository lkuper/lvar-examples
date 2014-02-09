-- A nondeterministic program.

import Control.Concurrent.Async
import Data.IORef
import Data.Set

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: IO (Set Item)
p = do cart <- newIORef empty
       async $ atomicModifyIORef cart (\s -> (insert Book s, ()))
       async $ atomicModifyIORef cart (\s -> (insert Shoes s, ()))
       res <- async $ readIORef cart
       wait res

main = do v <- p
          putStr $ show $ toList v
