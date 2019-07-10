import Chess
import Printer

main = do
  putStrLn "printing last position in positionTree after startPos"
  pretty . last $ Chess.positionTree Chess.startPosition
  putStrLn "completed"