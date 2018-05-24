# pandoc-filters

This repository contains the pandoc filters I use to convert different stages
of my PhD thesis.

Currently they are configured to work with pandoc 1.19.2.4. This version comes
with Ubuntu 18.04 and is compiled with pandoc-filters 1.17.0.5.

## Installation

    stack setup # if you want stack to install the right GHC version
    stack setup --system-ghc # if you have GHC 7.10.3 installed
    stack build
    stack install

The binaries will be installed to `$HOME/.local/bin`.

Make sure to have a pandoc version installed that fits the version of
pandoc-types required in `pandoc-filters.cabal`. ([related
issue](https://github.com/jgm/pandoc/issues/3217), pandoc's cabal file:
[pandoc.cabal](https://github.com/jgm/pandoc/blob/master/pandoc.cabal)).

Also make sure to compile the filter against the version of aeson your pandoc
uses ([related issue](https://github.com/jgm/pandoc/issues/3131)).
