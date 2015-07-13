{-# LANGUAGE TypeFamilies #-}

import Control.LVish
import qualified Control.Par.ST             as PST
import qualified Control.Par.ST.Vec         as V
import           Control.Monad              (void)
import           Data.Vector                (freeze, toList)

p :: (HasGet e, HasPut e) => PST.ParST (PST.MVectorFlp Int s1) Par e s [Int]
p = do
  V.set 0
  void (V.forkSTSplit 5 (V.write 0 5) (V.write 0 120))
  raw <- V.reify
  frozen <- PST.liftST (freeze raw)
  return (toList frozen)           

main = print (runPar (V.runParVecT 10 p))

