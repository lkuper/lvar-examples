{-

This example is a version of parallel-and-take-2.hs that uses a *real*
product lattice.

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
-- parameter, for session sealing, and the value it's storing.
type Result s = PureLVar s State

data State = State HalfState HalfState
  deriving (Eq, Ord, Show, Bounded)
           
data HalfState = Bot | T | F | Top
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Enum State where
  toEnum 0  = State Bot Bot
  toEnum 1  = State Bot T
  toEnum 2  = State Bot F
  toEnum 3  = State T    Bot
  toEnum 4  = State T    T
  toEnum 5  = State T    F
  toEnum 6  = State F    Bot
  toEnum 7  = State F    T
  toEnum 8  = State F    F
  toEnum 9  = State Top Top

  fromEnum (State Bot Bot) = 0
  fromEnum (State Bot T)    = 1
  fromEnum (State Bot F)    = 2
  fromEnum (State T    Bot) = 3
  fromEnum (State T    T)    = 4
  fromEnum (State T    F)    = 5
  fromEnum (State F    Bot) = 6
  fromEnum (State F    T)    = 7
  fromEnum (State F    F)    = 8
  fromEnum (State Top Top) = 9

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
  bottom = State Bot Bot

joinHalfStates :: HalfState -> HalfState -> HalfState
joinHalfStates x y | x == y = x
joinHalfStates Bot x = x
joinHalfStates Top x = Top
joinHalfStates T F = Top
joinHalfStates x y = joinHalfStates y x

-- The joinStates function computes the least upper bound of its
-- arguments.
joinStates :: State -> State -> State
joinStates (State x1 y1) (State x2 y2) =
  State (joinHalfStates x1 x2) (joinHalfStates y1 y2)
  

-- We should only be able to look at a `Result` when it's in one of
-- the "exactly enough information" states.
getResult :: Result s -> Par d s State
getResult res = do
  getPureLVar res [State T T, State F T,
                   State T F, State F F]

-- Now we can define an `asyncAnd` operation, which is the only way to
-- write to a `Result`.  It takes two values of type `Bool` (each
-- wrapped in a Par computation), creates a new LVar, launches two
-- threads that each write a `State` into the shared LVar, and finally
-- gets and returns the result.
asyncAnd :: Par d s Bool -> Par d s Bool -> Par d s Bool 
asyncAnd m1 m2 = do
  res <- newPureLVar (State Bot Bot)
  fork $ do b1 <- m1
            putPureLVar res
              (if b1 then (State T Bot) else (State F Bot))
--            liftIO $ putStrLn $ " [dbg], got left: " ++ show b1
  fork $ do b2 <- m2
            putPureLVar res
              (if b2 then (State Bot T) else (State Bot F))
--            liftIO $ putStrLn $ " [dbg], got right: " ++ show b2
  x <- getResult res
  return $! x == State T T

-- The main function just runs a bunch of calls to asyncAnd.
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

  -- Just testing...
  --putStrLn $ "Verify join lattice, should return Nothing: " ++ show (verifyFiniteJoin [(State Bot Bot) .. (State Top Top)] joinStates)
  --printAllJoins
  --testJoin

  -- Folding asyncAnd over a smallish list of alternating Trues and Falses.
  print $ runPar $
    foldr asyncAnd (return True)
    (concat $ replicate 10 [return True, return False])

  -- Folding asyncAnd over a list of alternating Trues and Falses.
  print $ runPar $
    foldr asyncAnd (return True)
    (concat $ replicate 100 [return True, return False])

  -- Here's a list of lots of Trues with a stray False in the middle.
  print $ runPar $ 
    foldr asyncAnd (return True)
    (concat [replicate 100 (return True), [return False],
             replicate 100 (return True)])

printAllJoins :: IO ()
printAllJoins = do
  print $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinStates x y)
    | x <- [(State Bot Bot) .. (State Top Top)],
      y <- [(State Bot Bot) .. (State Top Top)]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> show x ++ "\n" ++ showStrings xs

testJoin :: IO ()
testJoin = do
  print $ showStrings
    [show [v1, v2, v] ++ ": " ++
     show (if (v1 `joinLeq` v && v2 `joinLeq` v)
           then (v1 `join` v2) `joinLeq` v
           else True) -- if the premise doesn't hold, no problem
    | v1 <- [(State Bot Bot) .. (State Top Top)],
      v2 <- [(State Bot Bot) .. (State Top Top)],
      v  <- [(State Bot Bot) .. (State Top Top)]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> x ++ "\n" ++ showStrings xs
