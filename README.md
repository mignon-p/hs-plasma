This library is a Haskell binding to the libLoam and libPlasma C
libraries, which are in the [Plasma
repository](https://purl.org/funwithsoftware/libPlasma).  The
primary purpose is to bind the "pools" API from libPlasma,
although some useful libLoam functionality is also provided.

This library does not expose the "slaw" API from libPlasma.
Instead, marshaling of slawx is expected to be done with the
[hs-slaw](https://github.com/mignon-p/hs-slaw) Haskell library.

Pre-built Haddock documentation for `hs-slaw` and `hs-plasma` is
available [here](https://funwithsoftware.org/hs-plasma/).

This repository also contains the [slawcat](data/slawcat.pod) utility
program, which can be used to copy slawx between pools and/or files
from the command line.  A binary of `slawcat` for x86\_64 Linux is
available under
[releases](https://github.com/mignon-p/hs-plasma/releases).

## Prerequisites

Before building, you will need to have built and installed
the libLoam and libPlasma C libraries.  See
[their repository](https://purl.org/funwithsoftware/libPlasma)
for information.  You will need Plasma version 5.6.0 or later
(something downstream of
[97140008](https://github.com/plasma-hamper/plasma/pull/16)).

`hs-plasma.cabal` uses `pkg-config` to find the libLoam and libPlasma
libraries.  Therefore, the directory that contains `libLoam.pc` and
`libPlasma.pc` needs to be on your `PKG_CONFIG_PATH`.

You also need to have `perl` on your `PATH`, because Perl is used
to preprocess some files in `hs-slaw`.

## Building

The Haskell Plasma binding can be built with either
[Cabal](https://www.haskell.org/cabal/) or
[Stack](https://haskellstack.org/).  If you don't know anything about
Haskell, I recommend using Stack.  Just [install
Stack](https://docs.haskellstack.org/en/stable/#how-to-install-stack)
and then it will take care of downloading known-to-work versions of
[GHC](https://www.haskell.org/ghc/) and other tools automatically as
needed.

In either case, the hs-slaw and hs-plasma repositories should be
checked out as siblings:

```
cd ~/some/where
git clone https://github.com/mignon-p/hs-slaw.git
git clone https://github.com/mignon-p/hs-plasma.git
```

Then you'll want to go into the `plasma` directory to build both
libraries.  With Cabal:

```
cd hs-plasma
cabal v2-build
```

Or with Stack:

```
cd hs-plasma
stack build
```

The `cabal.project` file (in the case of Cabal) or the
`stack.yaml` file (in the case of Stack) tells the build system
to reach over into the sibling directory `hs-slaw` to get the
`hs-slaw` library that the `hs-plasma` library depends on.

You can run the self-test with:

```
cabal v2-test
```

or:

```
stack test
```

To install the executables (such as `slawcat`), do:

```
cabal v2-install
```

or:

```
stack install
```

By default, Cabal will install the executables in `~/.cabal/bin`, while Stack will install the executables in `~/.local/bin`.  So you'll want to make sure the appropriate directory is on your `PATH`.

To install the manpage for `slawcat`, run the `install-man.pl` script in this directory:

```
./install-man.pl
```

By default, it installs the manpage in `~/.local/share/man/man1`.  However, you can specify a `--prefix`, and then it will install into `$prefix/share/man/man1`.  For example:

```
sudo ./install-man.pl --prefix=/usr/local
```

or:

```
sudo ./install-man.pl --prefix=/opt/plasma
```

## License

`hs-plasma` is licensed under the [MIT License](LICENSE).
© Mignon Pelletier, 2024-2025.

Some documentation text has been taken from
[libPlasma](https://purl.org/funwithsoftware/libPlasma),
© oblong industries.
