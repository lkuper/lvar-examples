{-# LANGUAGE DataKinds #-}

import Control.LVish
import Control.LVish.DeepFrz
import Data.LVar.PureSet
import qualified Data.Set as S

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

-- Returns an ordinary Data.Set, because `freezeSet` turns a
-- `Data.LVar.PureSet` into one.
p :: Par QuasiDet s (S.Set Item)
p = do
  cart <- newEmptySet
  fork $ insert Book cart
  fork $ insert Shoes cart
  waitSize 2 cart
  freezeSet cart

main = do
  v <- runParIO p
  print v
