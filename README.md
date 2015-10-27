A library for computing with exact real numbers in Haskell.

## Prerequisites

* A generally working development environment (`make`, `gcc`)
* Probably the [GMP](https://gmplib.org) header files.
* Haskell version 7.8.4, including Cabal
* The [`devel-hmpfr` branch of Rounded](https://github.com/comius/rounded/tree/devel-hmpfr) library by [Ivo List](https://github.com/comius) which you can checkout with

       git clone git@github.com:comius/rounded.git
       cd rounded
       git checkout devel-hmpfr

The MPFR Haskell bindings in the Rounded library rely on low-level implementation details
of Haskell. It is likely they will break with newer versions of Haskell.

## Installation & Usage

1. Install the Rounded library, following the instructions there. Essentially, it is just

       cabal install

2. Run

       cabal install

   to install the library. To get a version of the library which allows profiling, use
   instead

       cabal install --ghc-option=-fprof-auto -p --enable-executable-profiling

3. Alternatively to the previous step, you can run

       cabal repl

   to get an interactive shell with the library preloaded. This way you can try out the
   library without installing it.

4. To create HTML documentation run

       cabal haddock

   The generated HTML files will be placed in `dist/doc/html/`.

To enable profiling of the library install with:
cabal install --ghc-option=-fprof-auto -p --enable-executable-profiling

## Example

Here is an example session.

    Preprocessing library haskell-fast-reals-0.1.0.0...
    GHCi, version 7.8.4: http://www.haskell.org/ghc/  :? for help
    <... stuff gets loaded ...>
    *Data.Reals.Reals> let a = 1.3 :: RealNum Rounded
    *Data.Reals.Reals> a*(1-a)
    [-3.900000000007822e-1,-3.899999999994179e-1]
    *Data.Reals.Reals> a^200
    [6.147102592468651e22,6.147102592468651e22]
    *Data.Reals.Reals> 
    *Reals> forall (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.26
    True
    *Reals> forall (ClosedInterval (0,1)) $ \x -> (x * (1 - x)) `less` exact 0.24
    False
