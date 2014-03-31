{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}
{-

This example is a version of parallel-and-take-2.hs that uses a *real*
product lattice.

-}

-- Make this a proper module, and export some stuff so we can play
-- with it in ghci.
module Main (asyncAnd, main, runPar, joinStates,
             printAllJoins, testJoin, verifyFiniteJoin) where

-- For debugging only.
import Control.LVish.Internal (liftIO)

-- Don't use `asyncAnd` from the LVish library, because we're going to
-- define our own version of it.
import Control.LVish hiding (asyncAnd, F)

import qualified Data.Set as S

-- Now let's import some stuff we need to define our own LVar.  We'll
-- be using the `PureLVar` type provided by Data.LVar.Internal.Pure,
-- along with some functions that operate on PureLVars and the
-- verifyFiniteJoin function.
import Data.LVar.Internal.Pure (PureLVar, newPureLVar, putPureLVar,
                                getPureLVarSets, verifyFiniteJoin)

-- We'll need this later.
import Algebra.Lattice hiding (top)
import Debug.Trace
--------------------------------------------------------------------------------

-- OK, we're ready to define the `Result` type: the type of the
-- result of a parallel logical "and" operation.

-- The PureLVar type constructor takes two arguments: the `s` type
-- parameter, for session sealing, and the value it's storing.
type Result s = PureLVar s State

-- data State = Top | Pr Bl Bl
type State = Maybe (Bl,Bl)
           
data Bl = Bot | T | F 
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Enum State where
  toEnum 0  = Just (Bot,Bot)
  toEnum 1  = Just (Bot,T)
  toEnum 2  = Just (Bot,F)
  toEnum 3  = Just (T, Bot)
  toEnum 4  = Just (T, T)
  toEnum 5  = Just (T, F)
  toEnum 6  = Just (F, Bot)
  toEnum 7  = Just (F, T)
  toEnum 8  = Just (F, F)
  toEnum 9  = Nothing
  fromEnum (Just (Bot,Bot))  = 0
  fromEnum (Just (Bot,T))    = 1
  fromEnum (Just (Bot,F))    = 2
  fromEnum (Just (T,   Bot)) = 3
  fromEnum (Just (T,   T))   = 4
  fromEnum (Just (T,   F))   = 5
  fromEnum (Just (F,   Bot)) = 6
  fromEnum (Just (F,   T))   = 7
  fromEnum (Just (F,   F))   = 8
  fromEnum Nothing           = 9



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
  bottom = Just (Bot,Bot)

top :: State
top = Nothing

joinBl :: Bl -> Bl -> Maybe Bl
joinBl x y | x == y = Just x
joinBl Bot x        = Just x
joinBl T F          = Nothing 
joinBl x y          = joinBl y x

-- The joinStates function computes the least upper bound of its
-- arguments.
joinStates :: State -> State -> State
joinStates a b = 
  do (x1,y1) <- a
     (x2,y2) <- b
     x3 <- joinBl x1 x2 
     y3 <- joinBl y1 y2
     return (x3,y3)

-- Equivalently, we could have written this:
joinStates2 Nothing _ = Nothing
joinStates2 _ Nothing = Nothing
joinStates2 (Just (x1,y1)) (Just (x2,y2)) = 
  do x3 <- joinBl x1 x2 
     y3 <- joinBl y1 y2
     return (x3,y3)
  
-- We should only be able to look at a `Result` when it's in one of
-- the "exactly enough information" states.
getResult :: HasGet e => Result s -> Par e s Bool
getResult res = do
  (tag,_) <- getPureLVarSets res [(False,allFalse), (True, S.singleton (Just (T,T)))]
  return $! tag

allFalse :: S.Set (Maybe (Bl, Bl))
allFalse = S.fromList $ filter falsey [bottom .. top]

falsey :: Maybe (Bl, Bl) -> Bool
falsey (Just (F,_)) = True  
falsey (Just (_,F)) = True
falsey _            = False

-- Now we can define an `asyncAnd` operation, which is the only way to
-- write to a `Result`.  It takes two values of type `Bool` (each
-- wrapped in a Par computation), creates a new LVar, launches two
-- threads that each write a `State` into the shared LVar, and finally
-- gets and returns the result.
asyncAnd :: (HasGet e, HasPut e) => 
            Par e s Bool -> Par e s Bool -> Par e s Bool 
asyncAnd m1 m2 = do
  res <- newPureLVar bottom
  fork $ do b1 <- m1
            putPureLVar res (Just (toBl b1,Bot))
--            liftIO $ putStrLn $ " [dbg], got left: " ++ show b1
  fork $ do b2 <- m2
            putPureLVar res (Just (Bot,toBl b2))
--            liftIO $ putStrLn $ " [dbg], got right: " ++ show b2
  getResult res

toBl :: Bool -> Bl
toBl True  = T
toBl False = F

-- The main function just runs a bunch of calls to asyncAnd.
main :: IO ()
main = do
  putStrLn "(1) First, a basic truth table:"
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

  putStr "(2) Now test short circuiting, one F on the left should unblock:  "
  print $ runPar $ 
     do v <- newPureLVar bottom
        putPureLVar v (Just (F,Bot))
        getResult v

  putStr "  And on the right: "
  print $ runPar $ 
     do v <- newPureLVar bottom
        putPureLVar v (Just (Bot,F))
        getResult v
  

  -- Just testing...
  --putStrLn $ "Verify join lattice, should return Nothing: " ++ show (verifyFiniteJoin [(State Bot Bot) .. (State Top Top)] joinStates)
  --printAllJoins
  --testJoin

  putStr "(3) Folding asyncAnd across a list: "
  putStr "  Folding asyncAnd over a smallish list of alternating Trues and Falses:  " 
  print $ runPar $
    foldr asyncAnd (return True)
    (concat $ replicate 10 [return True, return False])

  putStr "  Folding asyncAnd over a list of alternating Trues and Falses:  "
  print $ runPar $
    foldr asyncAnd (return True)
    (concat $ replicate 100 [return True, return False])

  putStr "  Here's a list of lots of Trues with a stray False in the middle:  " 
  print $ runPar $ 
    foldr asyncAnd (return True)
    (concat [replicate 100 (return True), [return False],
             replicate 100 (return True)])

printAllJoins :: IO ()
printAllJoins = do
  print $ showStrings
    ["join " ++ show x ++ " " ++ show y ++ " = " ++ show (joinStates x y)
    | x <- [bottom .. top],
      y <- [bottom .. top]]
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
    | v1 <- [bottom .. top],
      v2 <- [bottom .. top],
      v  <- [bottom .. top]]
  where showStrings strings = case strings of
          [] -> ""
          (x : xs) -> x ++ "\n" ++ showStrings xs
