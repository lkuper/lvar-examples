{-# LANGUAGE DataKinds #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureMap

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: Par Det s Int
p = do
  cart <- newEmptyMap
  fork $ insert Book 2 cart
  fork $ insert Shoes 1 cart
  getKey Book cart

main = do
  putStr $ show $ runPar p
