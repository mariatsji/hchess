module Main where

import qualified AI
import Data.Text (Text)
import NeatInterpolation
import PGN (parsePgn)
import Position (startPosition)
import Printer (pretty)

main :: IO ()
main = do
    -- let res = AI.bestDeepEval startPosition 2    16.8GB, 2.5s elapsed
    --let res = positionTree startPosition -- 600K
    let (_, _, status) = AI.bestDeepEval startPosition 2
    print status

debug :: IO ()
debug = do
    let Right pos = parsePgn testPgn
        (posM, _, _) = AI.bestDeepEval pos 2
    maybe (print @String "hmm") pretty posM

testPgn :: Text
testPgn =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "*"]

1. e2e3 d7d5 2. Qd1h5 Qd8d6 3. Bf1b5! Bc8d7 4. Bb5d3 g7g6 5. Qh5g5 Bf8h6 6. Qg5h4 Qd6f6 7. Qh4b4 Qf6b6 8. Qb4c3 d5d4 9. e3xd4 Qb6c6 10. Qc3xc6 Bd7xc6 11. f2f3 Nb8d7 12. Nb1c3 Nd7b6 13. d4d5 Nb6xd5 14. Bd3xg6 h7xg6 15. Ng1e2 Ng8f6 16. d2d3 Bh6xc1 17. Ra1xc1 Nd5xc3 18. Ne2xc3 O-O-O 19. Ke1f2 Nf6d5 20. Nc3e4 Nd5b4 21. Ne4c3 Nb4d5 22. Nc3e4 Nd5b4 23. Ne4c3 Nb4d5 24. Nc3e4 Nd5b4 25. Ne4c3 b7b6 26. a2a3 Nb4d5 27. Rc1b1 Kc8b7 28. Rh1e1 Nd5xc3 29. b2xc3 Rh8xh2 30. Re1xe7 Rh2h7 31. Rb1e1 Rd8d7 32. Re7e5 Rh7h5 33. Re5xh5 g6xh5 34. Re1h1 Bc6a4 35. Rh1xh5 Ba4xc2 36. Rh5b5 Rd7xd3 37. Kf2g3 Rd3xc3 38. Rb5b2 Rc3c6 39. Kg3f2 Bc2d3 40. f3f4 Rc6c2! 41. Rb2xc2 Bd3xc2 42. g2g4 Bc2d3 43. f4f5 f7f6 44. Kf2g3 Bd3e4 45. Kg3h4 Kb7a6 46. g4g5 Be4xf5 47. g5xf6 Bf5g6 48. Kh4h3 Ka6a5 49. Kh3h4 Ka5a4 50. Kh4g5 Bg6f7 51. Kg5h6 *
|]