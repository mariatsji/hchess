import           AI
import           Chess
import           Data.Bitraversable
import           Printer

main = do
  putStrLn "stream best depth 3 from startpos"
  bitraverse (pretty . fst) pretty (AI.streamBest Chess.startPosition 3)
