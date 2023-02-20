module PGNSpec where

import Chess (pieceAt)
import PGN (parsePgn, renderPgn)
import Position (Color (White), Piece (Pawn, King), Position (gamehistory), Square (Square), startPosition)

import NeatInterpolation
import Relude
import Test.Hspec
import Move (playMove)

spec :: Spec
spec = do
    describe "PGN" $ do
        it "renders PGN of a position, and back into a position" $ do
            let pgn = renderPgn startPosition
            parsePgn pgn `shouldBe` Right startPosition
        it "parses a long game PGN into a position" $ do
            let Right pos = parsePgn longPgn
            length (gamehistory pos) `shouldBe` 159
        it "parses an en passant PGN into a position" $ do
            let Right pos = parsePgn enPassant
            pieceAt pos (Square 1 5) `shouldBe` Nothing
            pieceAt pos (Square 1 6) `shouldBe` Just (Pawn White)
        it "can castle long of a specific position" $ do
            let Right pos = parsePgn castleLongFailed
                Right castled = playMove "O-O-O" pos
            pieceAt castled (Square 3 1) `shouldBe` Just (King White)


longPgn :: Text
longPgn =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "1-0"]

1. e2e3 e7e6 2. Qd1g4 Qd8f6 3. Nb1c3 h7h5 4. Qg4h3 Nb8c6 5. Bf1b5 Nc6a5 6. a2a3 Qf6e5 7. b2b4 Bf8xb4 8. a3xb4 b7b6 9. b4xa5 b6xa5 10. Ra1xa5 c7c6 11. Ng1f3 Qe5c5 12. Bc1a3 Qc5b6 13. Bb5xc6 Qb6xc6 14. Ra5c5 Qc6d6 15. Rc5xc8! Ra8xc8 16. Ba3xd6 Rc8c6 17. Bd6a3 Ng8f6 18. Ke1e2 Nf6g4 19. Nf3d4 Rc6c4 20. Ke2d3 Ng4e5! 21. Kd3e4 f7f6 22. Ba3d6 Ne5f7 23. Bd6a3 Nf7g5 24. Ke4d3 Rc4xc3! 25. d2xc3 Ng5xh3 26. g2xh3 e6e5 27. Nd4b5 a7a6 28. Nb5c7! Ke8f7 29. Nc7xa6 Rh8a8 30. f2f4 Ra8xa6 31. Ba3c5 d7d6 32. Bc5b4 Ra6b6 33. Bb4a5 Rb6b5 34. Ba5b4 e5e4! 35. Kd3xe4 d6d5! 36. Ke4d4 g7g5 37. Rh1a1 g5xf4 38. e3xf4 Rb5b7 39. Kd4xd5 Rb7b5! 40. Bb4c5 h5h4 41. Ra1a6 Kf7g6 42. c3c4 Rb5b2 43. Bc5d4 Rb2xc2 44. Ra6xf6! Kg6h5 45. Rf6f5! Kh5h6 46. Rf5f6! Kh6h5 47. Rf6f5! Kh5h6 48. Rf5f6! Kh6h5 49. Rf6f5! Kh5h6 50. Bd4f6 Rc2xh2 51. Bf6g5! Kh6g6 52. Rf5f6! Kg6g7 53. Bg5xh4 Rh2xh3 54. Bh4g5 Rh3h5 55. Rf6a6 Rh5h3 56. Bg5f6! Kg7f7 57. Ra6e6 Rh3h6 58. f4f5 Rh6xf6 59. Re6e5 Kf7f8 60. c4c5 Rf6f7 61. c5c6 Rf7f6 62. c6c7 Rf6d6! 63. Kd5xd6 Kf8f7 64. c7c8=Q Kf7f6 65. Qc8c4 Kf6g5 66. Kd6d7 Kg5f6 67. Kd7d6 Kf6g5 68. Kd6d7 Kg5f6 69. Kd7d6 Kf6g5 70. Kd6d7 Kg5f6 71. Re5a5 Kf6g5 72. Kd7e7 Kg5h5 73. Ra5a2 Kh5g5 74. Ke7e6 Kg5h5 75. Ke6f7 Kh5g5 76. Qc4e4 Kg5h5 77. Kf7g8 Kh5g5 78. Kg8g7 Kg5h5 79. f5f6 Kh5g5 80. Ra2a5# 1-0|]

enPassant :: Text
enPassant =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "*"]

1. b2b4 h7h5 2. b4b5 a7a5 3. b5xa6 *
    |]

castleLongFailed :: Text
castleLongFailed =
    [text|
[Event "hChess match"]
[Site "In front of computer"]
[Date "2022-12-13"]
[Round "1"]
[White "Humanoid Contender"]
[Black "Computer"]
[Result "*"]

1. c2c4 e7e6 2. d2d4 Qd8h4 3. Ng1f3 Bf8b4+ 4. Bc1d2 Bb4xd2+ 5. Qd1xd2 Qh4h5 6. Nb1c3 Qh5f5 *    
    |]