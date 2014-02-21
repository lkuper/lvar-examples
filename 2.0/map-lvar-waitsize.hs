{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Data.LVar.PureMap
import qualified Data.Map as M

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

-- Bug in LVish?: this program occasionally raises put-after-freeze
-- errors, even though I think the `waitSize 2` should be enough
-- synchronization to prevent that.

-- Returns an ordinary Data.Map, because `freezeMap` turns a
-- `Data.LVar.PureMap` into one.
p :: (HasPut e, HasGet e, HasFreeze e) => Par e s (M.Map Item Int)
p = do
  cart <- newEmptyMap
  fork $ insert Book 1 cart
  fork $ insert Shoes 1 cart
  waitSize 2 cart
  freezeMap cart

main = do
  v <- runParQuasiDet p
  print $ M.toList v
