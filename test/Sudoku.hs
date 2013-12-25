module Sudoku where

import Picosat ( solve, Solution(..) )

cross :: [a] -> [(a, a)]
cross list = [(x,y) | x <- list, y <- list]

rows, cols :: [[(Int, Int)]]
rows = [[(i,j) | i <- [0..8]] | j <- [0..8]]
cols = [[(i,j) | j <- [0..8]] | i <- [0..8]]

c2v :: (Int, Int, Int) -> Int
c2v (i,j,k) = (k-1)*81 + i*9 + j + 1

v2c :: Int -> (Int, Int, Int)
v2c x = ((i `mod` 81) `div` 9, i `mod` 9, (i `div` 81)+1)
  where i = x - 1

group :: [(Int, Int)] -> [[Int]]
group grp = foldr ((:).label) [] [1..9]
  where label k = map c2v [(i,j,k) | (i,j) <- grp ]

square :: [[(Int, Int)]]
square = [quadrent i j | (i,j) <- cross [0..2]]
  where quadrent x y = [(x*3+i,y*3+j) | (i,j) <- cross [0..2]]

oneLabel :: (Int, Int) -> [[Int]]
oneLabel (i,j) = atLeastOne : lessThan2
  where notBoth (c1,c2) = [- c2v (i,j,c1), - c2v (i,j,c2)]
        lessThan2  = map notBoth $ [(a,b) | (a,b) <- cross [1..9], a /= b]
        atLeastOne = map c2v [(i,j,k) | k <- [1..9]]

validLabeling :: [[Int]]
validLabeling = foldr ((++) . oneLabel) [] (cross [0..8])

goodLabeling :: [[Int]]
goodLabeling = foldr ((++) . group) [] (square ++ rows ++ cols)

sudokuForm :: [(Int, Int, Int)] -> [[Int]]
sudokuForm cells = validLabeling ++ goodLabeling ++ (map consClause cells)
  where consClause cell = [c2v cell]

getConstraints :: [[Int]] -> [(Int, Int, Int)]
getConstraints matrix = filter (\(_,_,a) -> a > 0) cells
  where flat = foldl1 (++) matrix
        cells = zip3 [i `div` 9 | i <- [0..]] (cycle [0..8]) flat

toSudoku :: [Int] -> [[Int]]
toSudoku [] = []
toSudoku sol = [[lkup i j | j <- [0..8]]
                          | i <- [0..8]]
  where
    cells = map v2c $ filter ((<) 0) sol
    lkup i j = label $ head $ filter (\(a,b,_) -> a == i && b == j) cells
    label (_,_,c) = c

stringToMatrix :: String -> [[Int]]
stringToMatrix = map (map $ read . return) . lines

puzzle :: String
puzzle = unlines [
      "004000000"
    , "000030002"
    , "390700080"
    , "400009001"
    , "209801307"
    , "600200008"
    , "010008053"
    , "900040000"
    , "000000800"
  ]

main :: IO ()
main = do
  let cnf = sudokuForm $ getConstraints $ stringToMatrix puzzle
  sol <- solve cnf
  case sol of
    Solution s -> mapM_ print $ toSudoku s
    _          -> putStrLn "Puzzle not solvable."
