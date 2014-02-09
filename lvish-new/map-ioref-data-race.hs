import Control.Concurrent.Async
import Data.IORef
import Data.Map

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: IO (Map Item Int)
p = do cart <- newIORef empty
       async $ atomicModifyIORef cart (\m -> (insert Book 1 m, ()))
       async $ atomicModifyIORef cart (\m -> (insert Shoes 1 m, ()))
       res <- async $ readIORef cart
       wait res

main = do v <- p
          putStr $ show $ toList v
