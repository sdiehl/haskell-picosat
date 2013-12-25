{-# LANGUAGE ForeignFunctionInterface #-}

module Picosat (
  solve,
  solveST,
  unsafeSolve,
  Solution(..)
) where

import Control.Monad

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST)

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall safe "picosat_init" picosat_init
    :: IO (Ptr a)

foreign import ccall safe "picosat_reset" picosat_reset
    :: Ptr a -> IO ()

foreign import ccall safe "picosat_add" picosat_add
    :: Ptr a -> CInt -> IO CInt

foreign import ccall safe "picosat_variables" picosat_variables
    :: Ptr a -> IO CInt

foreign import ccall safe "picosat_sat" picosat_sat
    :: Ptr a -> CInt -> IO CInt

foreign import ccall safe "picosat_deref" picosat_deref
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

toCInts :: Integral a => [[a]] -> [[CInt]]
toCInts = map $ map fromIntegral

solve :: Integral a => [[a]] -> IO Solution
solve cls = do
  let ccls = toCInts cls
  pico <- picosat_init
  _ <- addClauses pico ccls
  sol <- solution pico
  picosat_reset pico
  return sol

{-# NOINLINE solveST #-}
solveST :: Integral a => [[a]] -> ST t Solution
solveST = unsafeIOToST . solve

{-# NOINLINE unsafeSolve #-}
unsafeSolve :: Integral a => [[a]] -> Solution
unsafeSolve = unsafePerformIO . solve
