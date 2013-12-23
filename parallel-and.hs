{-

This example differs from most of those in this repo because we're
defining our own LVar in it, rather than using one of the built-in
LVar data types from the Data.LVar.* libraries.

When we define our own LVars, we're writing code that might break
determinism, so if this were a real program, we'd probably isolate all
the code that might break determinism from the code that cannot.
(Perversely, the code that might break determinism is called the
"trusted code", while the code that can't is "untrusted".)

Coming soon: a blog post explaining this code in detail.

-}

-- Make this a proper module, and export some stuff so we can play
-- with it in ghci.
module Main(asyncAnd, main, runPar, printAllJoins, testJoin) where

-- Don't use `asyncAnd` from the LVish library, because we're going to
-- define our own version of it.
import Control.LVish hiding (asyncAnd)

-- Now let's import some stuff we need to define our own LVar.  We'll
-- be using the `PureLVar` type provided by Data.LVar.Internal.Pure,
-- along with some functions that operate on PureLVars.
import Data.LVar.Internal.Pure (PureLVar, newPureLVar, putPureLVar, getPureLVar)

-- We'll need this later.
import Algebra.Lattice

-- OK, we're ready to define the `Result` type: the type of the
-- result of a parallel logical "and" operation.

-- The PureLVar type constructor takes two arguments: the `s` type
-- parameter, for session sealing, and the value it's storing, which
-- is in this case one of six variants.
type Result s = PureLVar s State
data State = Bot | TrueBot | BotTrue | TrueTrue | F | Top
           deriving (Eq, Ord, Show, Enum)

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


myLeq :: State -> State -> Bool
myLeq x y | x == y = True
myLeq Bot _ = True
myLeq _ Top = True

myLeq TrueBot TrueTrue = True
myLeq TrueBot F = True

myLeq BotTrue TrueTrue = True
myLeq BotTrue F = True

myLeq _ _ = False


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

-- Interesting cases involving `TrueTrue` or `F`.
joinStates TrueBot BotTrue = TrueTrue
joinStates TrueTrue TrueBot = TrueTrue
joinStates TrueTrue BotTrue = TrueTrue
joinStates F TrueTrue = Top
joinStates F _  = F

-- Join is commutative.
joinStates x y = joinStates y x

-- Now we're finally ready to define what the API to a `Result` will
-- be.  We should only be able to look at a `Result` when it's in one
-- of the two "exactly enough information" states: `TrueTrue` or `F`.
-- The definition of the `getResult` operation, which is written in terms of
-- the library function `getPureLVar` expresses this.  (In fact,
-- `getPureLVar` is one of the few places in LVish where "threshold
-- sets" appear explicitly: [TrueTrue, F] is a threshold set.)
getResult :: Result s -> Par d s State
getResult avr = getPureLVar avr [TrueTrue, F]

-- Now we can define an `asyncAnd` operation, which is the only way to
-- write to a `Result`.  It takes two values of type `Bool` (each
-- wrapped in a Par computation), creates a new LVar, launches two
-- threads that each write a `State` into the shared LVar, and finally
-- gets and returns the result.  If the result is neither `TrueTrue`
-- nor `F`, the `getResult` call will block.
asyncAnd :: Par d s Bool -> Par d s Bool -> Par d s Bool 
asyncAnd m1 m2 = do
  res <- newPureLVar Bot
  fork $ do b1 <- m1; putPureLVar res (if b1 then TrueBot else F)
  fork $ do b2 <- m2; putPureLVar res (if b2 then BotTrue else F)
  x <- getResult res
  return (x == TrueTrue)

-- The main function just runs a bunch of calls to asyncAnd, all of
-- which should return False.
main = do
 -- True and False result in False, of course.
 putStrLn $ show $ runPar $ 
   asyncAnd (return True) (return False)

 -- Folding asyncAnd over a big list of alternating Trues and Falses.
 putStrLn $ show $ runPar $ 
   foldr asyncAnd (return True) (concat $ replicate 100 [return True, return False])

 -- Here's a list of lots of Trues with a stray False in the middle.
 putStrLn $ show $ runPar $ 
   foldr asyncAnd (return True) (concat [replicate 100 (return True), [return False], replicate 100 (return True)])

-- Just for the sake of convincing ourselves that `joinStates` is
-- defined correctly, here's a function to print the result of calling
-- it for all 36 combinations of states.
printAllJoins = do
  putStrLn $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinStates x y)
    | x <- [Bot .. Top],
      y <- [Bot .. Top]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> show x ++ "\n" ++ showStrings xs

{- Example output of printAllJoins:

"join Bot Bot = Bot"
"join Bot TrueBot = TrueBot"
"join Bot BotTrue = BotTrue"
"join Bot TrueTrue = TrueTrue"
"join Bot F = F"
"join Bot Top = Top"
"join TrueBot Bot = TrueBot"
"join TrueBot TrueBot = TrueBot"
"join TrueBot BotTrue = TrueTrue"
"join TrueBot TrueTrue = TrueTrue"
"join TrueBot F = F"
"join TrueBot Top = Top"
"join BotTrue Bot = BotTrue"
"join BotTrue TrueBot = TrueTrue"
"join BotTrue BotTrue = BotTrue"
"join BotTrue TrueTrue = TrueTrue"
"join BotTrue F = F"
"join BotTrue Top = Top"
"join TrueTrue Bot = TrueTrue"
"join TrueTrue TrueBot = TrueTrue"
"join TrueTrue BotTrue = TrueTrue"
"join TrueTrue TrueTrue = TrueTrue"
"join TrueTrue F = Top"
"join TrueTrue Top = Top"
"join F Bot = F"
"join F TrueBot = F"
"join F BotTrue = F"
"join F TrueTrue = Top"
"join F F = F"
"join F Top = Top"
"join Top Bot = Top"
"join Top TrueBot = Top"
"join Top BotTrue = Top"
"join Top TrueTrue = Top"
"join Top F = Top"
"join Top Top = Top"

-}

{- The fishy part: This definition of `join` is not associative!

For associativity to hold, it has to be the case that

  x `join` (y `join` z) = (x `join` y) `join` z

However, according to the above definition of `join`, this is not the
case, because:

  TrueBot `join` (BotTrue `join` F) =
  TrueBot `join` F =
  F

  (TrueBot `join` BotTrue) `join` F =
  TrueTrue `join` F =
  Top

So our join-semilattice is not really a join-semilattice!

Now, the following are equivalent:

  (1) S is a join-semilattice.

  (2) For all elements x and y of S, the lub of the set {x, y} exists.

  (3) For all elements x, y, and z of S, the following properties hold:
  - Associativity: x `join` (y `join` z) == (x `join` y) `join` z
  - Commutativity: x `join` y == y `join` x
  - Idempotence: x `join` x == x

So it must be the case that (2) doesn't actually hold, even though we
wrote down the definition for `join`.  The `join` we wrote down must
not actually compute a least upper bound!

How can we tell if `join` actually computes a least upper bound?  It
would suffice for the following two properties to hold:

  * For all elements v1, v2, and v of S,
    if v1 <= v and v2 <= v, then (v1 `join` v2) <= v.

  * For all elements v1 and v2 of S,
    v1 <= (v1 `join` v2) and v2 <= (v1 `join` v2).

For us, the first of these does not hold! TrueBot <= F and BotTrue <=
F, but (TrueBot `join` BotTrue) == TrueTrue, which is not <= F.

Another way to put it: since `TrueTrue` and `F` are both upper bounds
of {`TrueBot`, `BotTrue`}, and neither `TrueTrue` nor `F` is less than
the other, then {`TrueBot`, `BotTrue`} doesn't actually have a lub,
regardless of what the definition of `join` claims it is.

-}

-- Here's some code to help check the first of the above two
-- properties.
testJoin = do
  putStrLn $ showStrings
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