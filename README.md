Haskell PicoSAT
---------------

Installing
----------

```bash
$ cabal install picosat
```

Usage
-----

haskell-picosat are Haskell bindings to the Picosat solver written
in C. It reads in clauses written in CNF and returns a solution if
satisfiable.

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
(A v ¬B v C) ^ (B v D v E) ^ (D v F)
```

Can be written as lists of zero-terminated integers:

```text
1 -2 3 0
2 4 5 0
4 6 0
```

```haskell
λ: import Picosat
λ: solve [[1, -2, 3], [2,4,5], [4,6]]
Solution [1,-2,3,4,5,6]
```

Which we can interpret as the solution:

```text
1   A 
-2 ¬B 
3   C
4   D
5   E
6   F
```

License
-------

PicoSAT itself is included and is also licensed the MIT license.
Copyright (c) 2006 - 2012, Armin Biere, Johannes Kepler University.

Released under the MIT License.
Copyright (c) 2013, Stephen Diehl
