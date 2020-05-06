import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import Picosat

printAllSolutions = do
  xs <- scopedAllSolutions
  liftIO $ mapM_ print xs

expectSolutions expected = do
  real <- scopedAllSolutions
  when (S.fromList expected /= S.fromList real) $ liftIO $ do
    print ("expected", expected)
    print ("real", real)
    error "test failed"

main =
  evalScopedPicosat $ do
    addBaseClauses [[1, 2, 3]]

    liftIO $ putStrLn "base cnf [[1, 2, 3]]"
    printAllSolutions
    expectSolutions
      [ Solution [1, 2, 3],
        Solution [1, 2, -3],
        Solution [1, -2, 3],
        Solution [1, -2, -3],
        Solution [-1, -2, 3],
        Solution [-1, 2, -3],
        Solution [-1, 2, 3]
      ]

    withScopedClauses [[-2, -3]] $ do
      liftIO $ putStrLn "\nwith [-2,-3]"
      printAllSolutions
      expectSolutions
        [ Solution [-1, 2, -3],
          Solution [-1, -2, 3],
          Solution [1, -2, -3],
          Solution [1, -2, 3],
          Solution [1, 2, -3]
        ]

    withScopedClauses [[-1, -2]] $ do
      liftIO $ putStrLn "\nwith [-1,-2]"
      printAllSolutions
      expectSolutions
        [ Solution [1, -2, 3],
          Solution [1, -2, -3],
          Solution [-1, -2, 3],
          Solution [-1, 2, -3],
          Solution [-1, 2, 3]
        ]

    addBaseClauses [[-1, -3]]
    expectSolutions
      [ Solution [-1, 2, -3],
        Solution [-1, 2, 3],
        Solution [-1, -2, 3],
        Solution [1, -2, -3],
        Solution [1, 2, -3]
      ]

    withScopedClauses [[-2, -3]] $ do
      expectSolutions
        [ Solution [1, -2, -3],
          Solution [1, 2, -3],
          Solution [-1, -2, 3],
          Solution [-1, 2, -3]
        ]

    withScopedClauses [[-1, -2], [1, -3]] $ do
      expectSolutions
        [ Solution [-1, 2, -3],
          Solution [1, -2, -3]
        ]

      let printSolutionsWithAssumptions as = do
            liftIO $ putStrLn ("\nwith assumptions " ++ show as)
            res <- scopedSolutionWithAssumptions as
            liftIO $ print res

      printSolutionsWithAssumptions [1]
      printSolutionsWithAssumptions [-1]
      printSolutionsWithAssumptions [2]
      printSolutionsWithAssumptions [-2]
      printSolutionsWithAssumptions [3]
      printSolutionsWithAssumptions [-3]
