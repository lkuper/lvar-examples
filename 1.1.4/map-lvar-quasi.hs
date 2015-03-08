{-# LANGUAGE DataKinds #-}

import Control.Concurrent (threadDelay)
import Control.LVish
import Control.LVish.Internal (liftIO)
import Data.LVar.PureMap
import qualified Data.Map as M

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: Par QuasiDet s (M.Map Item Int)
p = do
  cart <- newEmptyMap
  fork (insert Book 1 cart)
  fork (do liftIO (threadDelay 1); -- Might have to tweak this number to see quasi-determinism.
            insert Shoes 1 cart)
  freezeMap cart

main = do v <- runParIO p
          print v
