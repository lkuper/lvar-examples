import Control.Concurrent
import Data.Set as S

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: IO (Set Item)
p = do set <- newEmptyMVar
       forkIO $ putMVar set (fromList [Shoes])
       forkIO $ putMVar set (fromList [Book])
       takeMVar set

main = do
  v <- p
  putStr $ show $ toList v

-- Something like this could be an alternative to actually running the
-- program multiple times.
-- main = do
--   v <- replicateM 200 p
--   putStr $ show $ Prelude.map toList v

