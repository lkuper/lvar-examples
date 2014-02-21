{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureSet

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

instance DeepFrz Item where
  type FrzType Item = Item

p :: Par Det s (ISet s Item)
p = do
  cart <- newEmptySet
  fork $ insert Book cart
  fork $ insert Shoes cart
  return cart

main = print $ runParThenFreeze p
