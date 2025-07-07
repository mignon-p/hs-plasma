This library is a Haskell binding to the libLoam and libPlasma C libraries, which are in the [Plasma repository](https://purl.org/funwithsoftware/libPlasma).  The primary purpose is to bind the "pools" API from libPlasma, although some useful libLoam functionality is also provided.

This library does not expose the "slaw" API from libPlasma.  Instead, marshaling of slawx is expected to be done with the [slaw](https://github.com/mignon-p/hs-slaw) Haskell library.

## Prerequisites

Before building, you will need to have built and installed the libLoam and libPlasma C libraries.  See [their repository](https://purl.org/funwithsoftware/libPlasma) for information.

Note: Currently, we expect the libLoam and libPlasma C libraries to be installed in `/opt/plasma`.  Unfortunately, for now, this is hardcoded in the `include-dirs` and `extra-lib-dirs` of `plasma.cabal`.  In the future, we would like to use the `pkg-config` files, but I ran into some trouble with that, and this was expedient.

So, for example, the files `/opt/plasma/lib/libLoam.a` and `/opt/plasma/lib/libPlasma.a` are expected to exist.

## Building

The Haskell Plasma binding can be built with either [Cabal](https://www.haskell.org/cabal/) or [Stack](https://haskellstack.org/).  In either case, the hs-slaw and hs-plasma repositories should be checked out as siblings:

```
cd ~/some/where
git clone https://github.com/mignon-p/hs-slaw.git   slaw
git clone https://github.com/mignon-p/hs-plasma.git plasma
```

Then you'll want to go into the `plasma` directory to build both libraries.  With Cabal:

```
cd plasma
cabal v2-build
```

Or with Stack:

```
cd plasma
stack build
```

The `cabal.project` file (in the case of Cabal) or the `stack.yaml` file (in the case of Stack) tells the build system to reach over into the sibling directory `slaw` to get the `slaw` library that the `plasma` library depends on.

You can run the self-test with:

```
cabal v2-test
```

or:

```
stack test
```
