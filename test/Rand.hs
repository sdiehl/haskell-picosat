import Picosat
import RandomCNF (randomLiteral, randomCNF)

testNumSolutions num_vars negp clause_size num_clauses =
  do cnf <- randomCNF num_vars negp clause_size num_clauses
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


