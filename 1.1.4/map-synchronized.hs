import Control.Concurrent.Async
import Control.Concurrent
import Data.IORef
import Data.Map

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: IO (Map Item Int)
p = do cart <- newIORef empty
       a1 <- async $ atomicModifyIORef cart (\c -> (insert Book 1 c, ()))
       a2 <- async $ atomicModifyIORef cart (\c -> (insert Shoes 1 c, ()))
       res <- async $ do waitBoth a1 a2
                         readIORef cart
       wait res

main = do v <- p
          print v
