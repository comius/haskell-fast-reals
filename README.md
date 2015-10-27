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

1. Install the Rounded library, following the instructions there. Essentially, it is just a `cabal install` inside the Rounded source directory.

2. To install the library, run

        cabal install

   To get a version of the library which allows profiling, use instead

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
    *Data.Reals.Reals> a / a
    [9.999999999972715e-1,1.000000000001819]
    *Data.Reals.Reals> let b = 2.7 :: RealNum Rounded
    *Data.Reals.Reals> a * a - b * b - (a + b) * (a - b)
    [-3.128661774106285e-12,4.147295840103611e-12]
    *Data.Reals.Reals> a * a
    [1.689999999996871,1.690000000004147]
    *Data.Reals.Reals> approx (a * a) (precUp 100)
    [1.69,1.69]

