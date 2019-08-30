module Team
where

import Player

data Team = Team 
  { teamName :: String
  , players :: [Player]
  , teamScore :: Maybe Int
  }
  deriving (Eq)
