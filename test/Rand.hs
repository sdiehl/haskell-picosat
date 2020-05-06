import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word (Word64)
import Picosat
import RandomCNF (randomCNF, randomLiteral)
import System.CPUTime.Rdtsc

-- | Wrap any 'IO' computation so that it returns execution
-- time in CPU cycles as well as the real value.
measureCycles :: (MonadIO m) => m a -> m (Word64, a)
measureCycles ioa = do
  start <- liftIO $ rdtsc
  a <- ioa
  a `seq` return ()
  end <- liftIO $ rdtsc
  return (end - start, a)

-- | Test program demonstrating effectiveness of using Picosat's
-- solve-with-assumptions interface. One Picosat instance is kept
-- across many calls. It is able to use the kept state for good
-- improvements in run time.
--
-- Random CNF's are generated and then solved multiple times,
-- each time assuming another additional random literal.
-- On my computer the resulting times look like this:
-- @
-- -- 100 variables
-- ("num clauses",408)
-- ("unshared times:",[517,662,389,390,644,534,710,588,1808,
--     486,587,526,937,692,750,882,671,545,649,531,445])
-- ("shared times:",[460,185,25,23,21,20,19,20,22,153,203,25,
--     30,23,41,24,23,22,37,25,22])
-- -- ...
-- ("num clauses",418)
-- ("unshared times:",[1135,1384,913,1646,1753,2276,1277,1744,
--     1385,1552,1725,1909,1783,1463,715,1561,1802,1816,1660,1970,2145])
-- ("shared times:",[997,642,231,428,283,154,65,52,1,33,0,0,0,
--     0,0,0,0,0,0,0,0])
-- @
-- Unshared times are close to constant. This is not surprising. Each
-- time the same CNF plus an additional literal is solved again and
-- again from cold.  The shared times show how good Picosat runtimes
-- benefit from keeping one Picosat instance in memory.
testNumSolutions num_vars negp clause_size num_rands num_clauses =
  do
    cnf <- randomCNF num_vars negp clause_size num_clauses
    someLiterals <-
      mapM (\_ -> randomLiteral num_vars negp) [0 .. num_rands]
    print ("num clauses", num_clauses)
    let solveUnsharedWith r = do
          solution <- solve $ cnf ++ [[r]]
          case solution of
            Unsatisfiable -> return 0
            Solution _ -> return 1
    re <-
      mapM
        ( \r -> do
            (c, _) <- measureCycles (solveUnsharedWith r)
            return $ c `div` 3000
        )
        someLiterals
    print ("unshared times:", re)

    let solveSharedWith lit = do
          (cyc, _) <-
            measureCycles $
              scopedSolutionWithAssumptions [lit]
          return $ cyc `div` 3000
    ts <- evalScopedPicosat $ do
      addBaseClauses cnf
      mapM solveSharedWith someLiterals
    print ("shared times:", ts)

main = do
  mapM_ (testNumSolutions 100 0.5 3 20) [300, 302 .. 500]
