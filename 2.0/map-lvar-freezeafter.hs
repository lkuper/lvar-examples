{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureMap

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

-- LK: Do we actually need this instance?  We seem to be able to get
-- away without it!  Look into this.
-- instance DeepFrz Item where type
-- FrzType Item = Item

p :: (HasPut e, HasGet e) => Par e s (IMap Item s Int)
p = do
  cart <- newEmptyMap
  fork $ insert Book 1 cart
  fork $ insert Shoes 1 cart
  return cart

main = print $ runParThenFreeze p
