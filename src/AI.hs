module AI (first) where

import Chess

first :: GameHistory -> GameHistory
first gh = head (Chess.positionTree gh) : gh