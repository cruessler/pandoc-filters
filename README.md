# pandoc-filters

This repository contains the pandoc filters I use to convert different stages
of my PhD thesis.

Currently they are configured to work with pandoc 1.19.2.4. This version comes
with Ubuntu 18.04 and is compiled with pandoc-filters 1.17.0.5.

## Installation

    # if you want stack to install the right GHC version
    stack setup
    stack build
    stack install

    # if you want to use your system’s GHC
    stack setup --system-ghc
    stack build --system-ghc
    stack install --system-ghc

The binaries will be installed to `$HOME/.local/bin`.

Make sure to have a pandoc version installed that fits the version of
pandoc-types required in `pandoc-filters.cabal`. ([related
issue](https://github.com/jgm/pandoc/issues/3217), pandoc's cabal file:
[pandoc.cabal](https://github.com/jgm/pandoc/blob/master/pandoc.cabal)).

Also make sure to compile the filter against the version of aeson your pandoc
uses ([related issue](https://github.com/jgm/pandoc/issues/3131)).

## Updating to newer versions of pandoc

`pandoc-filters.cabal` has to be updated to have pandoc-types’ version match
the version your pandoc was compiled with. To get the correct version of
pandoc-types, run:

    $ pandoc version
    pandoc 1.19.2.4
    Compiled with pandoc-types 1.17.0.5, texmath 0.9.4.4, skylighting 0.3.3.1
    […]

To re-resolve package dependencies afterwards, run:

    stack solver --update-config --system-ghc

## Updating to newer versions of GHC

`stack.yaml` has a key `resolver` that tells Stack which GHC version to use.
To update that key (e. g. to use a newer version when your system’s GHC has
been updated), run:

    stack config set resolver 8.0

This updates `stack.yaml` to work with a new version.
