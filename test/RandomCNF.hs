module RandomCNF (randomLiteral,
                  randomClause,
                  randomCNF
                 ) where

import System.Random

randomLiteral :: Int -> Double -> IO Int
randomLiteral num_vars negp =
  do s <- randomRIO(0.0, 1.0)
     let sign = if s < negp then -1 else 1
     n <- randomRIO(1, num_vars)
     return $ sign * n

randomClause :: Int -> Double -> Int -> IO [Int]
randomClause num_vars negp clause_size =
  mapM (\_ -> randomLiteral num_vars negp) [0..clause_size]


randomCNF :: Int -> Double -> Int -> Int -> IO [[Int]]
randomCNF num_vars negp clause_size num_clauses =
  mapM (\_ -> randomClause num_vars negp clause_size) [0..num_clauses]

