import Picosat
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

randCNF num_vars negp clause_size num_clauses =
  mapM (\_ -> randomClause num_vars negp clause_size) [0..num_clauses]

testNumSolutions num_vars negp clause_size num_clauses =
  do cnf <- randCNF num_vars negp clause_size num_clauses
     xs <- solveAll cnf
     let nrandlit = do
           rlit <- randomLiteral num_vars negp
           ys <- solveAll $ cnf ++ [[rlit]]
           -- ys <- solveAllWithAssumptions cnf [rlit]
           return $ length ys
     ns <- mapM (\_->nrandlit) [0..10]
     print (num_clauses, length xs, ns)
     

main =
  mapM_ (testNumSolutions 17 0.5 3) [60..200]


