{-# LANGUAGE ForeignFunctionInterface #-}

{- |

We wish to find a solution that satisifes the following logical condition.

> (A v ¬B v C) ∧ (B v D v E) ∧ (D v F)

We can specify this as a zero-terminated lists of integers, with integers mapping onto the variable as ordered
in the condition and with integer negation corresponding to logical negation of the specific clause.

> 1 -2 3 0
> 2 4 5 0
> 4 6 0

We feed this list to the SAT solver using the 'solve' function either in IO or ST monad.

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
import Control.Monad.ST

main :: [Int]
main = runST $ do
  solveAllST [[1,2]]
  -- [Solution [1,2],Solution [-1,2],Solution [1,-2]]
@

For a higher level interface see: <http://hackage.haskell.org/package/picologic>

-}

module Picosat (
  solve,
  solveAll,
  unsafeSolve,
  unsafeSolveAll,
  Solution(..)
) where

import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall unsafe "picosat_init" picosat_init
    :: IO (Ptr a)

foreign import ccall unsafe "picosat_reset" picosat_reset
    :: Ptr a -> IO ()

foreign import ccall unsafe "picosat_add" picosat_add
    :: Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "picosat_variables" picosat_variables
    :: Ptr a -> IO CInt

foreign import ccall unsafe "picosat_sat" picosat_sat
    :: Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "picosat_deref" picosat_deref
    :: Ptr a -> CInt -> IO CInt

unknown, satisfiable, unsatisfiable :: CInt
unknown       = 0
satisfiable   = 10
unsatisfiable = 20

data Solution = Solution [Int]
              | Unsatisfiable
              | Unknown deriving (Show, Eq)

addClause :: Ptr a -> [CInt] -> IO ()
addClause pico cl = mapM_ (picosat_add pico) (cl ++ [0])

addClauses :: Ptr a -> [[CInt]] -> IO ()
addClauses pico = mapM_ (addClause pico)

getSolution :: Ptr a -> IO Solution
getSolution pico = do
  vars <- picosat_variables pico
  sol <- forM [1..vars] $ \i -> do
    s <- picosat_deref pico i
    return $ i * s
  return $ Solution $ map fromIntegral sol

solution :: Ptr a -> IO Solution
solution pico = do
  res <- picosat_sat pico (-1)
  case res of
    a | a == unknown       -> return Unknown
      | a == unsatisfiable -> return Unsatisfiable
      | a == satisfiable   -> getSolution pico
      | otherwise          -> error "Picosat error."

toCInts :: [[Int]] -> [[CInt]]
toCInts = map $ map fromIntegral

-- | Solve a list of CNF constraints yielding the first solution.
solve :: [[Int]] -> IO Solution
solve cls = do
  let ccls = toCInts cls
  pico <- picosat_init
  _ <- addClauses pico ccls
  sol <- solution pico
  picosat_reset pico
  return sol

-- | Solve a list of CNF constraints yielding all possible solutions.
solveAll :: [[Int]] -> IO [Solution]
solveAll e = do
  let e' = map (map fromIntegral) e
  s <- solve e'
  case s of
      Solution x -> (Solution x :) `fmap` solveAll (map negate x : e')
      _          -> return []

-- Unsafe solver functions are not guaranteed to be memory safe if the solver fails internally.

{-# NOINLINE unsafeSolve #-}
unsafeSolve :: [[Int]] -> Solution
unsafeSolve = unsafePerformIO . solve

{-# NOINLINE unsafeSolveAll #-}
unsafeSolveAll :: [[Int]] -> [Solution]
unsafeSolveAll = unsafePerformIO . solveAll
