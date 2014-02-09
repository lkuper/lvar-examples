{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Concurrent (threadDelay)
import Control.LVish
import Control.LVish.DeepFrz
import Control.LVish.Internal (liftIO)
import Data.LVar.PureMap
import qualified Data.Map as M

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: (HasPut e, HasGet e, HasFreeze e) => Par e s (M.Map Item Int)
p = do
  cart <- newEmptyMap
  fork $ insert Book 1 cart
  fork $ do liftIO $ threadDelay 1 -- Might have to tweak this number
                                   -- to see the quasi-determinism.
            insert Shoes 1 cart
  getKey Book cart -- Note the under-synchronization.
  freezeMap cart

main = do
  v <- runParQuasiDet p
  putStr $ show $ M.toList v
