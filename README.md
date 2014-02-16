# Programming with LVars, by example

[![Build Status](https://travis-ci.org/lkuper/lvar-examples.png?branch=master)](https://travis-ci.org/lkuper/lvar-examples)

_LVars_ are monotonically growing, lattice-based data structures for
deterministic parallel programming.
[LVish](http://hackage.haskell.org/package/lvish) is a Haskell library
for programming with LVars.  This repository contains toy examples of
programs that use LVars and LVish, as well as a few that _don't_ use
LVars but are there for illustrative purposes.

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

Then, depending on whether you have **lvish-1.1.2** or **lvish-2.0**
installed, go
[here](https://github.com/lkuper/lvar-examples/tree/master/1.1.2) or
[here](https://github.com/lkuper/lvar-examples/tree/master/2.0).
Enjoy!

## Caveats

The usual caveats about research code apply: **Parts of it are broken.
The API will change.  It will eat your laundry.**
