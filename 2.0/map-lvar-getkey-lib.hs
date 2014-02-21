{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import Data.LVar.PureMap

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: (HasPut e, HasGet e) => Par e s Int
p = do
  cart <- newEmptyMap
  fork $ insert Book 2 cart
  fork $ insert Shoes 1 cart
  getKey Book cart

main = do
  print $ runPar p
