{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- A silly example of what it might look like to roll our own map.
-- Keys are items, values are quantities. If we want to know how many
-- of a given item we have, we pass as a threshold set all the
-- possible quantities; the correct one will unblock and return.
-- which is a much more interesting kind of threshold than the kind
-- you can do on a set.

-- In this map, values are immutable (which is kind of silly for this
-- example, because you can't increase the quantity of an item that
-- you've already added a certain quantity of to your cart).  A more
-- interesting map would have the value also be an LVar -- perhaps a
-- counter!

import Algebra.Lattice
import Control.Concurrent (threadDelay)
import Control.LVish
import Control.LVish.Internal (liftIO)
import Data.LVar.Internal.Pure
import Data.Map as M (lookup, fromList)
import Data.Tuple (swap)

-- To keep it finite, we're assuming that five is the most you can get
-- of any item.  Bot is like a quantity of zero for everything.  This
-- is a silly way to define this; it would be better to have a count
-- of Zero be possible and have everything be a BookShoes.
data CartState = Bot
               | Book Count
               | Shoes Count
               | BookShoes Count Count
               | Top
  deriving (Show, Ord, Eq)

data Count = One | Two | Three | Four | Five
  deriving (Show, Ord, Eq, Enum)

countTranslation = [(One, 1), (Two, 2), (Three, 3), (Four, 4), (Five, 5)]

instance JoinSemiLattice CartState
         where join = joinCartStates

joinCartStates :: CartState -> CartState -> CartState
joinCartStates x y | x == y = x
joinCartStates Bot x = x
joinCartStates Top _ = Top
joinCartStates (Book x) (Book y) =
  if x == y then Book x else Top
joinCartStates (Shoes x) (Shoes y) =
  if x == y then Shoes x else Top
joinCartStates (Book x) (Shoes y) = BookShoes x y
joinCartStates (BookShoes x1 x2) (BookShoes y1 y2) =
  if (x1 == y1) && (x2 == y2) then (BookShoes x1 x2) else Top
joinCartStates (Book x1) (BookShoes x2 y2) =
  if (x1 == x2) then (BookShoes x2 y2) else Top
joinCartStates (Shoes y1) (BookShoes x2 y2) =
  if (y1 == y2) then (BookShoes x2 y2) else Top
joinCartStates x y = joinCartStates y x

-- A wrapper for putPureLVar.
putItem :: PureLVar s CartState -> (Count -> CartState) -> Int -> Par d s ()
putItem cart item count = putPureLVar cart (item x) where
  x = case M.lookup count $ M.fromList $ map swap countTranslation of
    Nothing -> undefined
    Just n -> n

-- A wrapper for getPureLVar.  Expects its first argument to be either
-- `Book` or `Shoes`.
getItemCount :: (Count -> CartState) -> PureLVar s CartState -> Par Det s Int
getItemCount item cart = do
  state <- getPureLVar cart [item x | x <- [One .. Five]]
  -- This is silly -- I wish I could match against multiple
  -- constructors at once.
  return (case state of
             Book x -> case M.lookup x $ M.fromList countTranslation of
               Nothing -> undefined
               Just n -> n
             Shoes x -> case M.lookup x $ M.fromList countTranslation of
               Nothing -> undefined
               Just n -> n)

p :: Par Det s Int
p = do
  cart <- newPureLVar Bot
  fork $ do --liftIO $ threadDelay 10000;
            putItem cart Book 1
  fork $ do --liftIO $ threadDelay 10000;
            putItem cart Shoes 2
  -- This threshold read is asking how many books were put in the
  -- cart.  It will deterministically return 1, regardless of whether
  -- shoes are in the cart yet or not (the actual CartState will be
  -- either (Book One) or (BookShoes One Two) at the time it unblocks,
  -- but we won't be able to distinguish those situations).
  getItemCount Book cart

main = print $ runPar p
