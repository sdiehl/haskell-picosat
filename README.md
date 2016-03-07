Haskell PicoSAT
---------------

[![Build Status](https://travis-ci.org/sdiehl/haskell-picosat.svg)](https://travis-ci.org/sdiehl/haskell-picosat)
[![Hackage](https://img.shields.io/hackage/v/picosat.svg)](https://hackage.haskell.org/package/picosat)

haskell-picosat are Haskell bindings to the PicoSAT solver, written in C. It reads in clauses in CNF (
Conjunctive-Normal Form ) and returns a solution which satisfies the clauses.

The most notable distinction of this binding is that the SAT solver library is included with the cabal package
so you shouldn't need to install anything but this package to get going. It's also notably faster than a pure
Haskell solution at solving very large constraint problems.

Installing
----------

```bash
$ cabal install picosat
```

Usage
-----

If we have a table of variables representing logical statements we can enumerate them with integers.

```text
A  1
B  2
C  3
D  4
E  5
F  6
```

Then the clause can be written as sequences of positive integers
(assertion) and negative integers (negation):

```text
(A v ¬B v C)
1 -2 3 0
```

```text
(B v D v E)
2 4 5 0
```

```text
(D V F)
4 6 0
```

Solutions to a statement of the form:

```text
(A v ¬B v C) ∧ (B v D v E) ∧ (D v F)
```

Can be written as zero-terminated lists of integers:

```text
1 -2 3 0
2 4 5 0
4 6 0
```

To use the Haskell bindings simply pass a list of clauses to
the ``solve`` function, this will return either the solution or
``Unsatisfiable`` or ``Unknown``.

```haskell
import Picosat

main :: IO [Int]
main = do
  solve [[1, -2, 3], [2,4,5], [4,6]]
  -- Solution [1,-2,3,4,5,6]
```

The solution given we can interpret as:

```text
1   A 
-2 ¬B 
3   C
4   D
5   E
6   F
```

To generate all possible solutions we repeatedly feed the negated solution to the solver yielding which is
implemented with the ``solveAll`` function which yields a sequence of solutions.

```haskell
import Picosat

main :: IO [Int]
main = solveAll [[1,2]]
  -- [Solution [1,2],Solution [-1,2],Solution [1,-2]]
```

For a more complicated example a Sudoku solver is included as an example.

License
-------

PicoSAT itself is included and is also licensed the MIT license.
Copyright (c) 2006 - 2012, Armin Biere, Johannes Kepler University.

Released under the MIT License.
Copyright (c) 2013-2016, Stephen Diehl
