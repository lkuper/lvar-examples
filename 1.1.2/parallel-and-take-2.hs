{-

This example is a version of parallel-and.hs that uses a product
lattice.  It recovers associativity but loses short-circuit behavior.

-}

-- Make this a proper module, and export some stuff so we can play
-- with it in ghci.
module Main(asyncAnd, main, runPar, joinStates,
            printAllJoins, testJoin, verifyFiniteJoin) where

-- For debugging only.
import Control.LVish.Internal (liftIO)

-- Don't use `asyncAnd` from the LVish library, because we're going to
-- define our own version of it.
import Control.LVish hiding (asyncAnd)

-- Now let's import some stuff we need to define our own LVar.  We'll
-- be using the `PureLVar` type provided by Data.LVar.Internal.Pure,
-- along with some functions that operate on PureLVars and the
-- verifyFiniteJoin function.
import Data.LVar.Internal.Pure (PureLVar, newPureLVar, putPureLVar,
                                getPureLVar, verifyFiniteJoin)

-- We'll need this later.
import Algebra.Lattice
import Debug.Trace
--------------------------------------------------------------------------------

-- OK, we're ready to define the `Result` type: the type of the
-- result of a parallel logical "and" operation.

-- The PureLVar type constructor takes two arguments: the `s` type
-- parameter, for session sealing, and the value it's storing, which
-- is in this case one of six variants.
type Result s = PureLVar s State

data State = Bot
           | TrueBot | BotTrue | FalseBot | BotFalse
           | TrueTrue | FalseTrue | TrueFalse | FalseFalse
           | Top
           deriving (Eq, Ord, Show, Enum, Bounded)

-- Library functions like `getPureLVar` and `putPureLVar` are defined
-- in terms of a `join` function on states of PureLVars, and they
-- express the expectation that such a `join` exists using
-- `JoinSemiLattice` typeclass constraints.  So, we'll need to provide
-- the join operation in our own code and declare `State` to be an
-- instance of `JoinSemiLattice`.
instance JoinSemiLattice State where
  join = joinStates

-- Additionally, `newPureLVar` takes a starting state as argument and
-- requires that it be an instance of `BoundedJoinSemiLattice`, so we
-- have to declare `State` to be an instance of that.  (The constraint
-- seems unnecessary to me, actually, but as long as it's there, we
-- need this instance declaration.)
instance BoundedJoinSemiLattice State where
  bottom = Bot

-- The joinStates function computes the least upper bound of its
-- arguments.
joinStates :: State -> State -> State
-- Joining an element with itself results in that element.
joinStates x y | x == y = x

-- Joining an element with `Bot` results in that element.
joinStates Bot x = x

-- Joining an element with `Top` results in `Top`, in either order.
joinStates Top _ = Top
joinStates _ Top = Top

-- Compatible pairs of states.
joinStates TrueBot BotTrue = TrueTrue
joinStates TrueTrue TrueBot = TrueTrue
joinStates TrueTrue BotTrue = TrueTrue

joinStates BotTrue FalseBot = FalseTrue
joinStates FalseTrue BotTrue = FalseTrue
joinStates FalseTrue FalseBot = FalseTrue

joinStates TrueBot BotFalse = TrueFalse
joinStates TrueFalse TrueBot = TrueFalse
joinStates TrueFalse BotFalse = TrueFalse

joinStates FalseBot BotFalse = FalseFalse
joinStates FalseFalse FalseBot = FalseFalse
joinStates FalseFalse BotFalse = FalseFalse

-- Incompatible pairs of states.
joinStates TrueBot FalseBot = Top
joinStates BotTrue BotFalse = Top

joinStates TrueTrue FalseTrue = Top
joinStates TrueTrue TrueFalse = Top
joinStates TrueTrue FalseFalse = Top

joinStates FalseTrue TrueFalse = Top
joinStates FalseTrue FalseFalse = Top

joinStates FalseTrue TrueBot    = Top
joinStates FalseTrue BotFalse   = Top
joinStates TrueFalse FalseFalse = Top
joinStates TrueFalse BotTrue    = Top

joinStates FalseFalse TrueBot = Top
joinStates FalseFalse BotTrue = Top

joinStates TrueTrue   FalseBot = Top
joinStates TrueTrue   BotFalse = Top
joinStates TrueFalse  FalseBot = Top

-- Join is commutative.
joinStates x y = joinStates y x


-- We should only be able to look at a `Result` when it's in one of
-- the "exactly enough information" states.
getResult :: Result s -> Par d s State
getResult avr = do
  getPureLVar avr [TrueTrue, FalseTrue, TrueFalse, FalseFalse]

-- Now we can define an `asyncAnd` operation, which is the only way to
-- write to a `Result`.  It takes two values of type `Bool` (each
-- wrapped in a Par computation), creates a new LVar, launches two
-- threads that each write a `State` into the shared LVar, and finally
-- gets and returns the result.
asyncAnd :: Par d s Bool -> Par d s Bool -> Par d s Bool 
asyncAnd m1 m2 = do
  res <- newPureLVar Bot
  fork $ do b1 <- m1
            putPureLVar res (if b1 then TrueBot else FalseBot)
--            liftIO $ putStrLn $ " [dbg], got left: " ++ show b1
  fork $ do b2 <- m2
            putPureLVar res (if b2 then BotTrue else BotFalse)
--            liftIO $ putStrLn $ " [dbg], got right: " ++ show b2
--  liftIO $ print "---"
  x <- getResult res
  return $! x == TrueTrue

-- The main function just runs a bunch of calls to asyncAnd, all of
-- which should return False.
main :: IO ()
main = do
  putStrLn "First, a basic truth table:"
  -- These all work, as well.
  putStr "  asyncAnd TT: "
  print $ runPar $ asyncAnd (return True) (return True)

  -- True and False result in False, of course.
  putStr "  asyncAnd TF: "
  print $ runPar $ asyncAnd (return True) (return False)

  putStr "  asyncAnd FT: "
  print $ runPar $ asyncAnd (return False) (return True)

  putStr "  asyncAnd FF: "
  print $ runPar $ asyncAnd (return False) (return False)

  -- These all either don't work or are really slow. :(

  putStrLn $ "Verify join lattice, should return Nothing: " ++ show (verifyFiniteJoin [Bot .. Top] joinStates)
  --printAllJoins
  --testJoin

  -- print $ runPar $ 
  --   foldr asyncAnd (return True) (concat $ replicate 10 [return True, return False])

  -- -- Folding asyncAnd over a big list of alternating Trues and Falses.
  -- print $ runPar $ 
  --   foldr asyncAnd (return True) (concat $ replicate 100 [return True, return False])

  -- -- Here's a list of lots of Trues with a stray False in the middle.
  -- print $ runPar $ 
  --   foldr asyncAnd (return True) (concat [replicate 100 (return True), [return False], replicate 100 (return True)])

printAllJoins = do
  print $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinStates x y)
    | x <- [Bot .. Top],
      y <- [Bot .. Top]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> show x ++ "\n" ++ showStrings xs

testJoin = do
  print $ showStrings
    [show [v1, v2, v] ++ ": " ++
     show (if (v1 `joinLeq` v && v2 `joinLeq` v)
           then (v1 `join` v2) `joinLeq` v
           else True) -- if the premise doesn't hold, no problem
    | v1 <- [Bot .. Top],
      v2 <- [Bot .. Top],
      v  <- [Bot .. Top]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> x ++ "\n" ++ showStrings xs
