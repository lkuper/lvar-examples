{-# LANGUAGE DataKinds #-}

-- Here's an undersynchronized, quasi-deterministic program that
-- either returns [Book, Shoes] or raises an error.

import Control.Concurrent (threadDelay)
import Control.LVish
import Control.LVish.DeepFrz
import Control.LVish.Internal (liftIO)
import Data.LVar.PureSet
import qualified Data.Set as S

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

p :: Par QuasiDet s (S.Set Item)
p = do
  cart <- newEmptySet
  fork $ insert Book cart
  fork $ do liftIO $ threadDelay 1 -- Might have to tweak this number
                                   -- to see the quasi-determinism.
            insert Shoes cart
  waitElem Book cart -- Note the under-synchronization.
  freezeSet cart

main = do
  v <- runParIO p
  print v
