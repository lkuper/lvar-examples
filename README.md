# Programming with LVars, by example

[![Build Status](https://travis-ci.org/lkuper/lvar-examples.png?branch=master)](https://travis-ci.org/lkuper/lvar-examples)

_LVars_ are monotonically growing, lattice-based data structures for
deterministic parallel programming.
[LVish](http://hackage.haskell.org/package/lvish) is a Haskell library
for programming with LVars.  This repository contains toy examples of
programs that use LVars and LVish, as well as a few that _don't_ use
LVars but are there for illustrative purposes.

## Prerequisites

The code has been [tested](https://travis-ci.org/lkuper/lvar-examples)
to build against GHC 7.8.1, 7.8.2, and 7.8.3.  You should probably use
one of those.

## Installing LVish

You can install LVish
[from Hackage](http://hackage.haskell.org/package/lvish) by running
`cabal install lvish` (perhaps preceded by `cabal update`).  That will
install the most recently released version, which, as of this writing,
is **lvish-1.1.4**.

If you want the newest version of lvish -- which has more cool
features than the 1.x releases do -- you'll have to install it from
git.  Something like this should work:

``` bash
git clone git@github.com:iu-parfunc/lvars.git
cd lvars/haskell/lvish/
cabal install
```

Then, depending on whether you have **lvish-1.1.4** or **lvish-2.0.x**
installed, go
[here](https://github.com/lkuper/lvar-examples/tree/master/1.1.4) or
[here](https://github.com/lkuper/lvar-examples/tree/master/2.0).
Enjoy!

## Caveats

The usual caveats about research code apply: **Parts of it are broken.
The API will change.  It will eat your laundry.**
