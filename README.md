# Programming with LVars, by example

_LVars_ are monotonically growing, lattice-based data structures for
deterministic parallel programming.
[LVish](http://hackage.haskell.org/package/lvish) is a Haskell library
for programming with LVars.  This repository contains toy examples of
programs that use LVars and LVish.

## Installing LVish

You can install LVish
[from Hackage](http://hackage.haskell.org/package/lvish) by running
`cabal install lvish` (perhaps preceded by `cabal update`).  If you
want the bleeding-edge version, you can get it from
[here](https://github.com/iu-parfunc/lvars/tree/master/haskell/lvish)
and then run `cabal install`:

``` bash
git clone git@github.com:iu-parfunc/lvars.git
cd lvars/haskell/lvish/
cabal install
```

The released version of LVish lags behind the head of the tree, and
some of the examples in this repository may not work against the
released version.  Sorry.

## Running the examples

The `Makefile` contains one target for each example program.  Each
target builds and then runs a program, often in an infinite loop to
illustrate the program's determinism or lack thereof.  For instance:

``` bash
$ make repeated-4-lvar
ghc -O2 repeated-4-lvar.hs -rtsopts -threaded
Linking repeated-4-lvar ...
while true; do ./repeated-4-lvar +RTS -N2; done
444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444^C
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
	 
### MaxCounter

  * `repeated-write-lvar-max-counter.hs`: Two threads race to write
     different integer values to an `LVar`.  Its contents
     deterministically end up as the maximum of the two writes.
	 
### Parallel logical "and"
  
  * `parallel-and.hs`: Two threads race to write to an `LVar` that
    stores the result of a parallel logical "and" operation.
	
### Shopping cart examples

  * **Nondeterministic**: `set-ioref-data-race.hs`. Two threads add
    items to a shopping cart, represented as an `IORef` containing a
    `Data.Set`, and a third thread reads the cart's contents, which
    are nondeterministic because the program is undersynchronized.
	
  * **Deterministic with no by-construction guarantee**:
    `set-synchronized.hs`. A revised version of the above program with
    enough synchronization to be deterministic, but only because it
    manually calls `wait` in the right places.
	
  * **Determinstic with a quasi-determinism-by-construction
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
	
### Not working yet

  * `pair-lvar.hs`: This one doesn't work, since LVish doesn't
    currently expose the `Pair` data type.  (We could, however,
    implement our own `Pair` using things that LVish provides if we
    wanted to.)

## Caveats

The usual caveats about research code apply: **Parts of it are broken.
The API will change.  It will eat your laundry.**
