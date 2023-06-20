import Data.List
import System.IO
import Data.Array



initialBoard :: Board
initialBoard = array ((1, 1), (8, 8)) $ do
  i <- [1..8]
  j <- [1..8]
  let piece = if (i + j) `mod` 2 == 0
                 then if i <= 3 then Black else if i >= 6 then White else Empty
                 else Empty
  return ((i, j), piece)






juego :: Tablero -> Int -> IO ()
juego t j = do
    limpiar
    escribeTablero t
    putChar '\n'
    putStr "Turno para el jugador "
    putStrLn (show j)
    -- pedimos la jugada
    (f, n) <- selecciona t
    -- ejecutamos la jugada
    let nt = jugada t f n
    -- comprobamos is ha ganado
    if finalizado nt then
        putStrLn ("El jugado " ++ (show j) ++ " ha ganado !!!")
    else do
        juego nt (siguiente j)
        return ()



