# Some toy `MVar`, `IVar`, and `LVar` programs

  * `data-race-example.hs`: An example of what we _don't_ want.  Two
    threads race to write different values to an `MVar`, resulting in
    a program that might print either evalue.
	
  * `ivar-example.hs`: An example of what we _do_ want.  Two threads
    race to write different values to an `IVar`, raising a `multiple
    put` error.
	
  * `ivar-multiple-write.hs`: An example of the limitations of
    `IVar`s.  Two threads race to write the same value to an `IVar`,
    raising a `multiple put` error.
	
  * `lvar-multiple-write.hs`: Two threads race to write the same value
     to an `LVar`, resulting in a program that deterministically
     prints that one value.
	 
If you want to be able to build `lvar-multiple-write.hs`, you will
need the `lattice-par` package, which you can get from
[here](https://github.com/iu-parfunc/lvars/tree/master/haskell/lattice-par)
and `cabal install`.  **Warning!  This is research code!  It will eat
your laundry!  If you like your Haskell environment just fine the way
it is, you might consider waiting until there's a released version.**
