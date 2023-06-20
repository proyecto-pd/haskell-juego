import Data.Array

data Piece = Empty | White | Black deriving (Eq, Show)

type Board = Array (Int, Int) Piece

initialBoard :: Board
initialBoard = array ((1, 1), (8, 8)) $ do
  i <- [1..8]
  j <- [1..8]
  let piece = if (i + j) `mod` 2 == 0
                 then if i <= 3 then White else if i >= 6 then Black else Empty
                 else Empty
  return ((i, j), piece)

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "   8 7 6 5 4 3 2 1"
  putStrLn "   A  B  C  D  E  F  G  H"
  let ((minRow, minCol), (maxRow, maxCol)) = bounds board
  mapM_ (printRow board) [maxRow, maxRow - 1..minRow]
  putStrLn "   A  B  C  D  E  F  G  H"
  

printRow :: Board -> Int -> IO ()
printRow board row = do
  putStr (show row ++ " ")
  let ((_, minCol), (_, maxCol)) = bounds board
  mapM_ (printCell board row) [minCol..maxCol]
  putStr (" " ++ show row)
  putStrLn ""

printCell :: Board -> Int -> Int -> IO ()
printCell board row col = do
  let piece = board ! (row, col)
  let backgroundColor = if (row + col) `mod` 2 == 0 then "\x1b[47m" else "\x1b[100m"
  let resetColor = "\x1b[0m"
  let cellContent = case piece of
        Empty -> " "
        White -> "w"
        Black -> "b"
  putStr (backgroundColor ++ " " ++ cellContent ++ " " ++ resetColor)

-- Ejemplo de uso:
main :: IO ()
main = printBoard initialBoard
