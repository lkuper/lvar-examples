# Some example `LVar` programs in Haskell

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

The `Makefile` contains one target for each example program.  Every targets both builds and runs an example, often in an infinite loop, to illustrate the example program's determinism or lack thereof.  An example run might look something like this:

``` bash
landin:lvar-examples lkuper$ make repeated-4-lvar
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
	
### Not working yet

  * `pair-lvar.hs`: This one doesn't work, since LVish doesn't
    currently expose the `Pair` data type.  (We could, however,
    implement our own `Pair` using things that LVish provides if we
    wanted to.)

## Caveats

The usual caveats about research code apply: **Parts of it are broken.
The API will change.  It will eat your laundry.**
