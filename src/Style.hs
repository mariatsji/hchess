module Style (Style (..), darkTheme, brightTheme) where

import qualified System.Console.ANSI as ANSI

darkTheme :: Style
darkTheme =
    Style
        { fonts = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black, ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.White]
        , lightSquare = [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue]
        , darkSquare = [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White]
        , emptySquare = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        , whitePiece = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        , blackPiece = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
        , highlightedSquare = [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Cyan]
        }

brightTheme :: Style
brightTheme =
    Style
        { fonts = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White, ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Black]
        , lightSquare = [ANSI.SetColor ANSI.Background ANSI.Vivid ANSI.Blue]
        , darkSquare = [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.White]
        , emptySquare = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Black]
        , whitePiece = [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.White]
        , blackPiece = [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Black]
        , highlightedSquare = [ANSI.SetColor ANSI.Background ANSI.Dull ANSI.Cyan]
        }

data Style = Style
    { fonts :: [ANSI.SGR]
    , darkSquare :: [ANSI.SGR]
    , lightSquare :: [ANSI.SGR]
    , emptySquare :: [ANSI.SGR]
    , whitePiece :: [ANSI.SGR]
    , blackPiece :: [ANSI.SGR]
    , highlightedSquare :: [ANSI.SGR]
    }