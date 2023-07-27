# Haskell Parse CSV using attoparsec

This example project parses a CSV file. If you need a full-blown CSV parser I
suggest the [cassava](http://hackage.haskell.org/package/cassava) package. This
is more an example project to learn features of the
[attoparsec](http://hackage.haskell.org/package/attoparsec) package. Note that
[cassava](https://hackage.haskell.org/package/cassava#readme) uses
[attoparsec](http://hackage.haskell.org/package/attoparsec).

To get started read [Haskell Financial Data Modelling and Predictive Analytics
by Pavel
Ryzhov](https://www.packtpub.com/big-data-and-business-intelligence/haskell-financial-data-modeling-and-predictive-analytics).

## Build

The project can uses [Cabal](#cabal) and/or [Stack](#stack) to build.

### Cabal

Build using [Cabal](https://www.haskell.org/cabal/) with:

```bash
make -f cabal.mk [target]
```

If [cabal.mk](cabal.mk) is linked to [Makefile](Makefile) we can just call:

```bash
make [target]
```

### Stack

Build using [Stack](https://docs.haskellstack.org/en/stable/) with:

```bash
make -f stack.mk [target]
```

If [stack.mk](stack.mk) is linked to [Makefile](Makefile) we can just call:

```bash
make [target]
```

## Haddock Documentation

Both make files include a target to build
[Haddock](http://hackage.haskell.org/package/haddock) documentation.

The documentation is publish to https://frankhjung1.gitlab.io/haskell-csv/.
