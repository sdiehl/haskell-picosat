{-# LANGUAGE ForeignFunctionInterface #-}

{- |

We wish to find a solution that satisifes the following logical condition.

> (A v ¬B v C) ∧ (B v D v E) ∧ (D v F)

We can specify this as a zero-terminated lists of integers, with integers mapping onto the variable as ordered
in the condition and with integer negation corresponding to logical negation of the specific clause.

> 1 -2 3 0
> 2 4 5 0
> 4 6 0

We feed this list of clauses to the SAT solver using the 'solve' function.

@
import Picosat

main :: IO [Int]
main = do
  solve [[1, -2, 3], [2,4,5], [4,6]]
  -- Solution [1,-2,3,4,5,6]
@

The solution given we can interpret as:

>  1  A
> -2 ~B
>  3  C
>  4  D
>  5  E
>  6  F

To generate all satisfiable solutions, use 'solveAll' function.:

@
import Picosat

main :: IO [Int]
main = solveAll [[1,2]]
  -- [Solution [1,2],Solution [-1,2],Solution [1,-2]]
@

For a higher level interface see: <http://hackage.haskell.org/package/picologic>

-}

module Picosat (
  solve,
  solveAll,
  unsafeSolve,
  unsafeSolveAll,
  Picosat,
  Solution(..)
) where

import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "picosat_init" picosat_init
    :: IO (Picosat)

foreign import ccall unsafe "picosat_reset" picosat_reset
    :: Picosat -> IO ()

foreign import ccall unsafe "picosat_add" picosat_add
    :: Picosat -> CInt -> IO CInt

foreign import ccall unsafe "picosat_variables" picosat_variables
    :: Picosat -> IO CInt

foreign import ccall unsafe "picosat_sat" picosat_sat
    :: Picosat -> CInt -> IO CInt

foreign import ccall unsafe "picosat_deref" picosat_deref
    :: Picosat -> CInt -> IO CInt

foreign import ccall unsafe "picosat_push" picosat_push
    :: Picosat -> IO CInt

foreign import ccall unsafe "picosat_pop" picosat_pop
    :: Picosat -> IO CInt

type Picosat = Ptr ()

-- | Call a monadic action with a freshly created Picosat that
-- is destroyed afterwards.
withPicosat :: (Picosat -> IO a) -> IO a
withPicosat f = do
  pico <- picosat_init
  res <- f pico
  picosat_reset pico
  return res
  
unknown, satisfiable, unsatisfiable :: CInt
unknown       = 0
satisfiable   = 10
unsatisfiable = 20

data Solution = Solution [Int]
              | Unsatisfiable
              | Unknown deriving (Show, Eq)

addClause :: Picosat -> [CInt] -> IO ()
addClause pico cl = mapM_ (picosat_add pico) (cl ++ [0])

addClauses :: Picosat -> [[CInt]] -> IO ()
addClauses pico = mapM_ (addClause pico)

getSolution :: Picosat -> IO Solution
getSolution pico = do
  vars <- picosat_variables pico
  sol <- forM [1..vars] $ \i -> do
    s <- picosat_deref pico i
    return $ i * s
  return $ Solution $ map fromIntegral sol

solution :: Picosat -> IO Solution
solution pico = do
  res <- picosat_sat pico (-1)
  case res of
    a | a == unknown       -> return Unknown
      | a == unsatisfiable -> return Unsatisfiable
      | a == satisfiable   -> getSolution pico
      | otherwise          -> error "Picosat error."

clauseToCInts :: [Int] -> [CInt]
clauseToCInts = map fromIntegral

cnfToCInts :: [[Int]] -> [[CInt]]
cnfToCInts = map clauseToCInts

-- | Solve a list of CNF constraints yielding the first solution.
solve :: [[Int]] -> IO Solution
solve cls = do
  let ccls = cnfToCInts cls
  withPicosat $ \ pico -> do
    _ <- addClauses pico ccls
    sol <- solution pico
    return sol

-- | Solve a list of CNF constraints yielding all possible solutions.
solveAll :: [[Int]] -> IO [Solution]
solveAll cls = do
  withPicosat $ \pico -> do
    let ccls = cnfToCInts cls
    let allSolutions solutions = do
          sol <- solution pico
          case sol of
            Solution ys -> do
              addClause pico $ clauseToCInts $ map negate ys
              allSolutions (sol : solutions)
            _ ->
              return $ reverse solutions
    _ <- addClauses pico ccls
    solutions <- allSolutions []
    return solutions

-- Unsafe solver functions are not guaranteed to be memory safe if the solver fails internally.

{-# NOINLINE unsafeSolve #-}
unsafeSolve :: [[Int]] -> Solution
unsafeSolve = unsafePerformIO . solve

{-# NOINLINE unsafeSolveAll #-}
unsafeSolveAll :: [[Int]] -> [Solution]
unsafeSolveAll = unsafePerformIO . solveAll
