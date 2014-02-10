# lvish-1.1.2 examples

These examples all build and run against **lvish-1.1.2**.  In
particular, they use "determinism levels" instead of the finer-grained
"effect levels" used in lvish-2.0 and later.  They are **not** going
to work against lvish-2.0 and later.  For that, you want
[this directory](https://github.com/lkuper/lvar-examples/tree/master/2.0).

## Building and running the examples

To build one of the example programs, run `cabal build` followed by
the name of the executable.  For instance, to build `repeated-4-lvar`
and then run it a few hundred times in a loop (to illustrate its
determinism), you might do as follows:

``` bash
$ cabal build repeated-4-lvar
Preprocessing executable 'repeated-4-lvar' for lvar-examples-old-0.1.0.0...
[1 of 1] Compiling Main             ( repeated-4-lvar.hs, dist/build/repeated-4-lvar/repeated-4-lvar-tmp/Main.o )
Linking dist/build/repeated-4-lvar/repeated-4-lvar ...
$ while true; do ./dist/build/repeated-4-lvar/repeated-4-lvar; done
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444^C
```

## What the examples do

### Comparing LVars with IVars and MVars

These five examples are documented in
[a blog post](http://composition.al/blog/2013/09/22/some-example-mvar-ivar-and-lvar-programs-in-haskell/):

  * `data-race-example.hs`: An example of what we _don't_ want.  Two
    threads race to write different values to an `MVar`, resulting in
    a program that might print either evalue.
	
  * `ivar-example.hs`: An example of what we _do_ want.  Two threads
    race to write different values to an `IVar`, raising a `multiple
    put` error.
	
  * `repeated-4-ivar.hs`: An example of the limitations of `IVar`s.
    Two threads race to write the same value to an `IVar`, raising a
    `multiple put` error.
	
  * `repeated-4-lvar.hs`: Two threads race to write the same value to
     an `LVar`, resulting in a program that deterministically prints
     that one value.
	 
  * `repeated-write-lvar-wrong.hs`: Two threads race to write
     _conflicting_ values to an `LVar`, resulting in a program that
     deterministically raises an error.
	 
### Parallel logical "and"

This example is documented in a blog post,
["The LVar that wasn't"](http://composition.al/blog/2013/12/24/the-lvar-that-wasnt/).
  
  * `parallel-and.hs`: Two threads race to write to an `LVar` that
    stores the result of a parallel logical "and" operation.  But
    something is fishy...
	
### Shopping cart examples

#### Sets

  * **Nondeterministic**: `set-ioref-data-race.hs`. Two threads add
    items to a shopping cart, represented as an `IORef` containing a
    `Data.Set`, and a third thread reads the cart's contents, which
    are nondeterministic because the program is undersynchronized.
	
  * **Deterministic with no by-construction guarantee**:
    `set-synchronized.hs`. A revised version of the above program with
    enough synchronization to be deterministic, but only because it
    manually calls `wait` in the right places.
	
  * **Deterministic with a quasi-determinism-by-construction
    guarantee**: `set-lvar-waitsize.hs`. A version of the shopping
    cart program where we represent the cart as a `Data.LVar.PureSet`
    of `Item`s (where an `Item` is either a `Book` or `Shoes`, because
    what else would you want to buy?).  Synchronizes on the size of
    the cart with a call to `waitSize`, then freezes and returns the
    cart's contents.  It is guaranteed to be quasi-deterministic
    (which is already a stronger guarantee than we have of the `IORef`
    version), and in fact it is deterministic (but only because we put
    the call to `waitSize` in the right place).  Note that we have to
    run the computation with `runParIO` rather than `runPar` because
    only guaranteed-deterministic computations can be run with
    `runPar`, and this one is only guaranteed quasi-deterministic as
    far as Haskell knows.
	
  * **Deterministic with a determinism-by-construction guarantee, but
    boring**: `set-lvar-waitelem.hs`. A version of the shopping cart
    program where, instead of using `Data.LVar.PureSet`, we roll our
    own LVar type using a `PureLVar` and our own `CartState` type and
    `joinCartStates` operation.  It is guaranteed to be deterministic,
    and it thresholds on the `Both` state; no explicit barrier
    operations required.  (But this isn't a particularly interesting
    threshold -- ask for `Both`, get `Both`.)
	
  * **Deterministic with a determinism-by-construction guarantee, and
    interesting**: `set-lvar-freezeafter.hs`. So far, all our LVar
    examples have used `runPar`; this time, we're using
    `runParThenFreeze`.  We add `Item`s to a `Data.LVar.PureSet`
    inside a `Par` computation, then run that computation with
    `runParThenFreeze`, which lets us look at the frozen set contents.
    The computation is deterministic, because it never tries to read
    from the LVar; we only read once the LVar has been frozen and
    converted to an ordinary `Data.Set`, and at that point no more
    LVar operations can occur.  No synchronization operations needed,
    and no chance of a write after freeze!  (This works because
    `runPar` is implicitly a global barrier, and `runParThenFreeze`
    just calls `runPar`.)
	 
  * **Quasi-deterministic with a quasi-determinism-by-construction
    guarantee**: `set-lvar-quasi.hs`. This example shows off
    quasi-determinism.  In it, we manually freeze the shopping cart
    after adding the `Item`s, and we intentionally under-synchronize
    by waiting for the `Book` but not the `Shoes`.  Because the
    computation does a freeze, it is guaranteed to be
    quasi-deterministic, but not deterministic, and so we have to run
    it with `runParIO`.
	
#### Maps

Maps are more interesting than sets to threshold on because it's
possible to threshold on the key and get back the value.

  * `map-ioref-data-race.hs`
  
  * `map-synchronized.hs`
  
  * `map-lvar-waitsize.hs`

  * `map-lvar-getkey.hs`: An experiment in implementing our own map
    LVar using a `PureLVar`, much as `set-lvar-waitelem.hs` does for
    sets.  The encoding is hilariously baroque, but the idea is that
    threshold reads are more interesting than for sets: instead of
    just finding out that the element you asked for is now in the set,
    with maps it's possible to get back the value associated with a
    key, by providing a threshold set of _all the values_ that might
    be associated with that key.  (This only works if the map values
    are immutable, though!  An even more interesting map would have
    LVars as values.)
	
  * `map-lvar-getkey-lib.hs`: Does the same thing as the above, but
    uses the built-in LVish `PureMap` instead of rolling our own LVar.

  * `map-lvar-freezeafter.hs`

  * `map-lvar-quasi.hs`
  
### Graph traversal examples

  * `graph-traversal-explicit-freeze.hs`
  
  * `graph-traversal-implicit-freeze.hs`

