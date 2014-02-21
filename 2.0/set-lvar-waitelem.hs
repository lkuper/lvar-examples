{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Here we define our own LVar using `PureLVar` and our own
-- `CartState` type, and do a threshold read of the cart.

-- `getPureLVar cart [Shoes, Book]` would be an illegal threshold set
-- because `Shoes` and `Book` have `Both` as their least upper bound.
-- But `[Shoes]` alone, for instance, would be fine.

-- For a fun time, uncomment the `threadDelay` calls.

-- This program raises occasional MVar exceptions due to a bug in
-- LVish.  This isn't a bug with the LVish interface but something
-- deeper and more sinister.

import Algebra.Lattice
import Control.Concurrent (threadDelay)
import Control.LVish
import Control.LVish.Internal (liftIO)
import Data.LVar.Internal.Pure

data CartState =
  Bot | Book | Shoes | Both | Top
  deriving (Show, Ord, Eq)

instance JoinSemiLattice CartState
         where join = joinCartStates

joinCartStates :: CartState -> CartState -> CartState
joinCartStates x y | x == y = x
joinCartStates Bot x = x
joinCartStates Top _ = Top
joinCartStates _ Top = Top
joinCartStates Book Shoes = Both
joinCartStates Book Both = Both
joinCartStates Shoes Both = Both
joinCartStates x y = joinCartStates y x

p :: (HasPut e, HasGet e) => Par e s CartState
p = do
  cart <- newPureLVar Bot
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart Book
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart Shoes
  getPureLVar cart [Both]

main = print $ runPar p
