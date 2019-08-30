module Game
where

import Player
import Team

import System.Random
import Control.Monad.IO.Class

data Game = Game
  { team1 :: Team
  , team2 :: Team
  }

data GameResults = GameResults
  { winner :: Team
  , loser :: Team
  , winningScore :: Int
  , losingScore :: Int
  , points :: [Points]
  --, points :: Player
  }

instance Show GameResults where
  show g = do
    let w = winner g
    let l = loser g
    "Team: " ++ (teamName w) ++ " beat " ++ (teamName l) ++ " with a score of " ++ (show $ winningScore g) ++ " - " ++ (show $ losingScore g)

data Point = Point
  { scoringTeam :: Team
  , assist :: Player
  , goal :: Player
  , turnoversBy :: [Player]
  , blocksBy :: [Player]
  }

data Action = Score | Block
  deriving (Show, Enum, Bounded)

data GameState = GameState
  { points :: [Point]
  , teamInPossession :: Team
  }


removePlayer _ [] = []
removePlayer player (p:ps) | player == p = removePlayer player ps
                           | otherwise = p : removePlayer player ps

randomOtherPlayer player team = randomPlayer (removePlayer player team)

randomPlayer team = do
  let p = players team
  g <- getStdGen
  return $ p !! (head (randomRs (0,(length p) - 1) g))

-- generate a point, until one team has 15
randomPoints game = do
  p <- randomPoint


--generateIncidentName :: HandlerFor App [Char]
--generateWinnner team1 team2 = do
  --g <- getStdGen
  --scoringPlayers (randoms g :: [Player])
  --return $ "incident_" ++ take 10 (randomRs ('a', 'z') g)

-- TODO - generate random points - this should be a game generator
play :: Game -> IO GameResults
play game = do
  p <- randomPoints game
  --p <- randomPlayer $ team1 game
  return $ GameResults
    { winner = team1 game
    , loser = team2 game
    , winningScore = 15
    , losingScore = 7
    , points = p
    }
  --, points = randomPoints team1 team2

