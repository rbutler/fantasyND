module Point
where

import Team
import Player

data Point = Point
  { scoringTeam :: Team
  , assist :: Player
  , goal :: Player
  , turnoversBy :: [Player]
  , blocksBy :: [Player]
  , homeTeamPoints :: Int
  , awayTeamPoints :: Int
  }
  deriving (Eq)

instance Show Point where
  show p = "Point: " ++ (show $ scoringTeam p) ++ " scored.  Home team has: " ++ (show $ homeTeamPoints p) ++ ". Away team has: " ++ (show $ awayTeamPoints p) ++ ".\n"
