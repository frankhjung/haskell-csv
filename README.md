# Haskell Parse CSV using attoparsec

This example project parses a CSV file. If you need a full-blown CSV parser I'd
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

## Run

Run against example data using:

```bash
@cat "files/asx.csv" | stack exec -- main -
```

Or simply:

```bash
make exec
```

```bash
$ make exec
Quote {qDate = 2018-08-05 00:00:00, qStock = "AFI", qShares = 5060.0, qBasis = 5.021, qPrice = 6.37}
Quote {qDate = 2018-08-05 00:00:00, qStock = "ANN", qShares = 146.0, qBasis = 20.718, qPrice = 28.76}
Quote {qDate = 2018-08-05 00:00:00, qStock = "ANZ", qShares = 1381.0, qBasis = 25.156, qPrice = 28.64}
Quote {qDate = 2018-08-05 00:00:00, qStock = "AST", qShares = 3000.0, qBasis = 0.0, qPrice = 1.62}
Quote {qDate = 2018-08-05 00:00:00, qStock = "COH", qShares = 100.0, qBasis = 59.823, qPrice = 203.84}
Quote {qDate = 2018-08-05 00:00:00, qStock = "CSR", qShares = 5000.0, qBasis = 1.751, qPrice = 4.17}
Quote {qDate = 2018-08-05 00:00:00, qStock = "DLX", qShares = 1000.0, qBasis = 0.436, qPrice = 7.67}
Quote {qDate = 2018-08-05 00:00:00, qStock = "IOO", qShares = 360.0, qBasis = 54.4, qPrice = 65.17}
Quote {qDate = 2018-08-05 00:00:00, qStock = "MIR", qShares = 5060.0, qBasis = 2.19, qPrice = 2.65}
Quote {qDate = 2018-08-05 00:00:00, qStock = "MQG", qShares = 200.0, qBasis = 50.43, qPrice = 121.62}
Quote {qDate = 2018-08-05 00:00:00, qStock = "NZM", qShares = 643.0, qBasis = 0.0, qPrice = 0.77}
Quote {qDate = 2018-08-05 00:00:00, qStock = "ORI", qShares = 1000.0, qBasis = 13.062, qPrice = 17.45}
Quote {qDate = 2018-08-05 00:00:00, qStock = "STW", qShares = 856.0, qBasis = 53.638, qPrice = 58.42}
Quote {qDate = 2018-08-05 00:00:00, qStock = "TLS", qShares = 6000.0, qBasis = 4.208, qPrice = 2.82}
Quote {qDate = 2018-08-05 00:00:00, qStock = "VAF", qShares = 400.0, qBasis = 48.496, qPrice = 48.5}
Quote {qDate = 2018-08-05 00:00:00, qStock = "VAP", qShares = 278.0, qBasis = 76.94, qPrice = 80.63}
Quote {qDate = 2018-08-05 00:00:00, qStock = "VSO", qShares = 2.0, qBasis = 52.38, qPrice = 57.2}
Quote {qDate = 2018-08-05 00:00:00, qStock = "WES", qShares = 1090.0, qBasis = 26.784, qPrice = 49.84}
Quote {qDate = 2018-08-05 00:00:00, qStock = "WOW", qShares = 2029.0, qBasis = 22.261, qPrice = 29.72}
Number of Quotes read: 19
Average price: 42.94
```

## Documentation

Both make files include a target to build
[Haddock](http://hackage.haskell.org/package/haddock) documentation.

The documentation is publish to https://frankhjung1.gitlab.io/haskell-csv/.

## Repositories

This project is available from both:

- https://github.com/frankhjung/haskell-csv
- https://gitlab.com/frankhjung1/haskell-csv
