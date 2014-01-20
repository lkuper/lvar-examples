{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- A silly example of what it might look like to roll our own map.
-- It's just a pair, and we pass as a threshold set all the possible
-- values that might go with the key.  One of them is returns, so we
-- can in essence threshold on key and get value back -- which is a
-- much more interesting kind of threshold than the kind you can do on
-- a set.

-- Values are immutable.  A more interesting map would have the value
-- also be an LVar!

import Algebra.Lattice
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.LVish
import Control.LVish.Internal (liftIO)
import Data.LVar.Internal.Pure

-- To keep it finite, we're assuming that two is the most you can get
-- of any item.  Bot is like a quantity of zero for everything.
data CartState = Bot
               | Book Count
               | Shoes Count
               | BookShoes Count Count
               | Top
  deriving (Show, Ord, Eq)

data Count = One | Two
  deriving (Show, Ord, Eq)

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

getBookCount :: PureLVar s CartState -> Par Det s CartState
getBookCount cart = getPureLVar cart [Book One, Book Two]

p :: Par Det s CartState
p = do
  cart <- newPureLVar Bot
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart (Book One)
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart (Shoes Two)
  -- This threshold set says, "Tell me how many books are in the
  -- cart."  It will return BookOne, regardless of whether ShoesTwo is
  -- written yet or not (the actual contents will be either (Book One)
  -- or (BookShoes One Two) at the time it unblocks).
  getBookCount cart

main = do
  -- printAllJoins
  putStr $ show $ runPar p

printAllJoins = do
  putStrLn $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinCartStates x y)
    | x <- allStates,
      y <- allStates]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> show x ++ "\n" ++ showStrings xs
        allStates = [Bot,
                     Book One,
                     Book Two,
                     Shoes One,
                     Shoes Two,
                     BookShoes One One,
                     BookShoes One Two,
                     BookShoes Two One,
                     BookShoes Two Two,
                     Top]
