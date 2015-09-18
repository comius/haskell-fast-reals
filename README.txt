To create HTML documentation run

   cabal haddock

To enable profiling of the library install with:
cabal install --ghc-option=-fprof-auto -p --enable-executable-profiling

To run the package for real number computation, run cabal repl.
Here is an example session.

Preprocessing library haskell-fast-reals-0.1.0.0...
GHCi, version 7.8.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package rounded-0.1 ... linking ... done.
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package containers-0.5.5.1 ... linking ... done.
Loading package binary-0.7.1.0 ... linking ... done.
Loading package filepath-1.3.0.2 ... linking ... done.
Loading package old-locale-1.0.0.6 ... linking ... done.
Loading package time-1.4.2 ... linking ... done.
Loading package unix-2.7.0.1 ... linking ... done.
Loading package directory-1.2.1.0 ... linking ... done.
Loading package pretty-1.1.1.1 ... linking ... done.
Loading package process-1.2.0.0 ... linking ... done.
Loading package Cabal-1.22.1.1 ... linking ... done.
unknown option: 'c'
[1 of 7] Compiling Data.Approximate.ApproximateField ( src/Data/Approximate/ApproximateField.hs, interpreted )
[2 of 7] Compiling Data.Approximate.Floating.MPFR ( src/Data/Approximate/Floating/MPFR.hs, interpreted )
[3 of 7] Compiling Data.Reals.Staged ( src/Data/Reals/Staged.hs, interpreted )
[4 of 7] Compiling Data.Reals.Space ( src/Data/Reals/Space.hs, interpreted )
[5 of 7] Compiling Data.Approximate.Interval ( src/Data/Approximate/Interval.hs, interpreted )
[6 of 7] Compiling Data.Reals.Lipschitz ( src/Data/Reals/Lipschitz.hs, interpreted )
[7 of 7] Compiling Data.Reals.Reals ( src/Data/Reals/Reals.hs, interpreted )
Ok, modules loaded: Data.Reals.Lipschitz, Data.Approximate.ApproximateField, Data.Approximate.Floating.MPFR, Data.Reals.Staged, Data.Approximate.Interval, Data.Reals.Space, Data.Reals.Reals.
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
