import AI
import Chess
import Position
import Printer

main = do
  let Right best = edgeGreed startPosition 3
  pretty best
          
