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


If you intend to solve a set of similar CNFs think about using
Picosat's incremental interface. It allows to push and pop
sets of clauses, as well as solving under assumptions.

@
import Picosat (evalScopedPicosat, addBaseClauses,
                withScopedClauses, scopedAllSolutions,
                scopedSolutionWithAssumptions)

main :: IO [Int]
main =
  evalScopedPicosat $ do
    addBaseClauses [[1, 2, 3]]
    -- == [Solution [1,2,3],
    --     Solution [1,2,-3],
    --     Solution [1,-2,3],
    --     Solution [1,-2,-3],
    --     Solution [-1,-2,3],
    --     Solution [-1,2,-3],
    --     Solution [-1,2,3]]

    withScopedClauses [[-2,-3]] $ do
      sol <- scopedAllSolutions
      -- ==   [Solution [-1,2,-3],
      --       Solution [-1,-2,3],
      --       Solution [1,-2,-3],
      --       Solution [1,-2,3],
      --       Solution [1,2,-3]]

    addBaseClauses [[-1,-3]]

    withScopedClauses [[-1,-2], [1,-3]] $ do
      sol <- scopedSolutionWithAssumptions [1]
@


-}

module Picosat (
  solve,
  solveAll,
  unsafeSolve,
  unsafeSolveAll,
  Picosat,
  Solution(..),
  evalScopedPicosat,
  addBaseClauses,
  withScopedClauses,
  scopedAllSolutions,
  scopedSolutionWithAssumptions
) where

import Control.Monad
import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr
import Foreign.C.Types

import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import qualified Data.Set as S

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

foreign import ccall unsafe "picosat_context" picosat_context
    :: Picosat -> IO CInt

foreign import ccall unsafe "picosat_assume" picosat_assume
    :: Picosat -> CInt -> IO ()


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
              | Unknown deriving (Show, Eq, Ord)

addClause :: Picosat -> [Int] -> IO ()
addClause pico cl = do
  _ <- mapM_ (picosat_add pico . fromIntegral) cl
  _ <- picosat_add pico 0
  return ()

addClauses :: Picosat -> [[Int]] -> IO ()
addClauses pico = mapM_ $ addClause pico

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

-- | Solve a list of CNF constraints yielding the first solution.
solve :: [[Int]] -> IO Solution
solve cnf = do
  withPicosat $ \ pico -> do
    _ <- addClauses pico cnf
    sol <- solution pico
    return sol

-- | Solve a list of CNF constraints yielding all possible solutions.
solveAll :: [[Int]] -> IO [Solution]
solveAll cnf = do
  evalScopedPicosat $ do
    addBaseClauses cnf
    scopedAllSolutions


data PicosatScoped = PicosatScoped { psPicosat :: Picosat,
                                     psContextVars :: S.Set Int }

type PS a = StateT PicosatScoped IO a

evalScopedPicosat :: PS a -> IO a
evalScopedPicosat action =
  withPicosat $ \ picosat -> do
    evalStateT action $ PicosatScoped picosat S.empty

addBaseClauses :: [[Int]] -> PS ()
addBaseClauses clauses = do
  pico <- gets psPicosat
  liftIO $ addClauses pico clauses

withScopedClauses :: [[Int]] -> PS a -> PS a
withScopedClauses clauses action = do
  pico <- gets psPicosat
  withScope $ do
    liftIO $ addClauses pico clauses
    action

withScope :: PS a -> PS a
withScope action = do
  pico <- gets psPicosat
  contextVars0 <- gets psContextVars
  ctx <- liftIO $ picosat_push pico
  addContextVariable $ fromIntegral ctx
  res <- action
  _ <- liftIO $ picosat_pop pico
  -- no -- modify $ \s -> s { psContextVars = contextVars0 }
  return res

addContextVariable :: Int -> PS ()
addContextVariable var = modify add
  where add s = s { psContextVars = S.insert var $ psContextVars s}

-- | Get one solution in scoped context. Pay attention to not
-- return any "context variable" which are Picosat internals.
scopedSolution :: PS Solution
scopedSolution = do
  pico <- gets psPicosat
  sol <- liftIO $ solution pico
  case sol of
    Solution ys -> do
      ctxvars <- gets psContextVars
      return $ Solution $
        filter (\l -> S.notMember (abs l) ctxvars) $ ys
    x ->
      return x


scopedAllSolutions :: PS [Solution]
scopedAllSolutions = do
  let recur solutions = do
        pico <- gets psPicosat
        sol <- scopedSolution
        case sol of
          Solution ys -> do
            let negsol = map negate ys
            liftIO $ addClause pico negsol
            recur (sol : solutions)
          _ ->
            return $ reverse solutions
  withScope $ recur []
  

scopedSolutionWithAssumptions :: [Int] -> PS Solution
scopedSolutionWithAssumptions assumptions = do
  pico <- gets psPicosat
  liftIO $ mapM_ (picosat_assume pico . fromIntegral) assumptions
  scopedSolution


-- Unsafe solver functions are not guaranteed to be memory safe if the solver fails internally.

{-# NOINLINE unsafeSolve #-}
unsafeSolve :: [[Int]] -> Solution
unsafeSolve = unsafePerformIO . solve

{-# NOINLINE unsafeSolveAll #-}
unsafeSolveAll :: [[Int]] -> [Solution]
unsafeSolveAll = unsafePerformIO . solveAll
