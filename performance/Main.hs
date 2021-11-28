import AI
import Position
import Printer

main = do
  pretty $
    case edgeGreed startPosition 5 of
      Left (pos, status) -> pos
      Right pos -> pos
          