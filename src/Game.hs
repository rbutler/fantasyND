module Game
where

import Player
import Team

data Game = Game
  { team1 :: Team
  , team2 :: Team
  }

data GameResults = GameResults
  { winner :: Team
  , loser :: Team
  , winningScore :: Int
  , losingScore :: Int
  }


play :: Game -> GameResults
play g =
  GameResults
  { winner = team1 g
  , loser = team2 g
  , winningScore = 15
  , losingScore = 7
  }

