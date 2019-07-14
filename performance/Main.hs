import           AI
import           Chess
import           Printer

main = do
  putStrLn "expanding position 4 from startpos"
  pretty . last $ (AI.expandHorizon 4) Chess.startPosition
  putStrLn "completed"
