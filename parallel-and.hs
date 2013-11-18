-- This example differs from most of those in this repo because we're
-- defining our own LVar in it.  Ordinarily, we'd be doing that in its
-- own file.  Why?  Because when we define our own LVars, we're
-- writing code that might break determinism, whereas when we write
-- code that merely *uses* LVars, we're writing code that, although it
-- could do various bad things, can't break determinism.  Therefore we
-- ought to be isolating all the code that might break determinism
-- from the code that cannot.  (For inane historical reasons, the code
-- that might break determinism is called the "trusted code".  Don't
-- get me started.)

-- Don't use the `asyncAnd` from the real LVish library, because we're
-- going to define our own version of that, too.
import Control.LVish hiding (asyncAnd)

-- Now let's import some stuff we need to define our own LVar.
import Algebra.Lattice
import Data.LVar.Internal.Pure (PureLVar, newPureLVar, putPureLVar, getPureLVar)

type AndVar s = PureLVar s AndState
data AndState = Bot | TrueBot | BotTrue | TrueTrue | F
              deriving (Eq, Ord, Show, Enum)

instance BoundedJoinSemiLattice AndState where
  bottom = Bot

instance JoinSemiLattice AndState where
  join TrueBot BotTrue = TrueTrue
  join TrueTrue TrueBot = TrueTrue
  join TrueTrue BotTrue = TrueTrue
  join Bot x = x
  join F  _  = F
  join x y = join y x 

-- Unblock only in one of these two states:
getAnd :: AndVar s -> Par d s AndState
getAnd avr = getPureLVar avr [TrueTrue,F]

-- Parallel logical "and" with early answers:
asyncAnd :: Par d s Bool -> Par d s Bool -> Par d s Bool 
asyncAnd m1 m2 = do
  avr <- newPureLVar Bot
  fork $ do b1 <- m1; putPureLVar avr (if b1 then TrueBot else F)
  fork $ do b1 <- m2; putPureLVar avr (if b1 then BotTrue else F)
  x <- getAnd avr
  return (x == TrueTrue)

-- FIXME: I don't work yet.
main = do
  putStrLn $ show $ runPar $
    asyncAnd (return True) (return False)
--    foldr asyncAnd (return False) (concat $ replicate 100 [return True, return False]) 
