{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureMap

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

instance DeepFrz Item where
  type FrzType Item = Item

p :: Par Det s (IMap Item s Int)
p = do
  cart <- newEmptyMap
  fork $ insert Book 1 cart
  fork $ insert Shoes 1 cart
  return cart

main = do
  print $ runParThenFreeze p
