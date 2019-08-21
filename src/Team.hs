module Team
where

import Player

data Team = Team 
  { teamName :: String
  , players :: [Player]
  , score :: Maybe Int
  }
