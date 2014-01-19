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

data Item = Book | Shoes
  deriving (Show, Ord, Eq)

-- To keep it finite, we're assuming that two is the most you can get
-- of any item.  Bot is like a quantity of zero for everything.
data CartState = Bot
               | BookOne
               | BookTwo
               | ShoesOne
               | ShoesTwo
               | BookOneShoesOne
               | BookOneShoesTwo
               | BookTwoShoesOne
               | BookTwoShoesTwo
               | Top
  deriving (Show, Ord, Eq, Enum)

instance JoinSemiLattice CartState
         where join = joinCartStates

-- This is very, very silly.
joinCartStates :: CartState -> CartState -> CartState
joinCartStates x y | x == y = x
joinCartStates Bot x = x
joinCartStates Top _ = Top
joinCartStates _ Top = Top

joinCartStates BookOne ShoesOne = BookOneShoesOne
joinCartStates BookOne ShoesTwo = BookOneShoesTwo
joinCartStates BookTwo ShoesOne = BookTwoShoesOne
joinCartStates BookTwo ShoesTwo = BookTwoShoesTwo

joinCartStates BookOne BookTwo = Top
joinCartStates ShoesOne ShoesTwo = Top

joinCartStates BookOneShoesOne BookOne = BookOneShoesOne
joinCartStates BookOneShoesOne ShoesOne = BookOneShoesOne
joinCartStates BookOneShoesTwo BookOne = BookOneShoesTwo
joinCartStates BookOneShoesTwo ShoesTwo = BookOneShoesTwo
joinCartStates BookTwoShoesOne BookTwo = BookTwoShoesOne
joinCartStates BookTwoShoesOne ShoesOne = BookTwoShoesOne
joinCartStates BookTwoShoesTwo BookTwo = BookTwoShoesTwo
joinCartStates BookTwoShoesTwo ShoesTwo = BookTwoShoesTwo

joinCartStates BookOneShoesOne BookTwo = Top
joinCartStates BookOneShoesOne ShoesTwo = Top
joinCartStates BookOneShoesTwo BookTwo = Top
joinCartStates BookOneShoesTwo ShoesOne = Top
joinCartStates BookTwoShoesOne BookOne = Top
joinCartStates BookTwoShoesOne ShoesTwo = Top
joinCartStates BookTwoShoesTwo BookOne = Top
joinCartStates BookTwoShoesTwo ShoesOne = Top

joinCartStates BookOneShoesOne BookOneShoesTwo = Top
joinCartStates BookOneShoesOne BookTwoShoesOne = Top
joinCartStates BookOneShoesOne BookTwoShoesTwo = Top

joinCartStates BookOneShoesTwo BookTwoShoesOne = Top
joinCartStates BookOneShoesTwo BookTwoShoesTwo = Top

joinCartStates BookTwoShoesOne BookOneShoesTwo = Top
joinCartStates BookTwoShoesOne BookTwoShoesTwo = Top

joinCartStates x y = joinCartStates y x

p :: Par Det s CartState
p = do
  cart <- newPureLVar Bot
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart BookOne
  fork $ do --liftIO $ threadDelay 10000;
            putPureLVar cart ShoesTwo
  -- This threshold set says, "Tell me how many books are in the
  -- cart."  It will return BookOne, regardless of whether ShoesTwo is
  -- written yet or not (the actual contents will be either BookOne or
  -- BookOneShoesTwo at the time it unblocks).
  getPureLVar cart [BookOne, BookTwo]

main = do
  -- printAllJoins
  putStr $ show $ runPar p

-- This helped me get joinCartStates right.
printAllJoins = do
  putStrLn $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinCartStates x y)
    | x <- [Bot .. Top],
      y <- [Bot .. Top]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> show x ++ "\n" ++ showStrings xs
