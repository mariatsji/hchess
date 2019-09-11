import AI
import Chess
import Data.Bitraversable
import Position
import Printer

main = do
  putStrLn "stream best depth 3 from startpos"
  bitraverse (pretty . fst) pretty (AI.streamBest startPosition 3)
