module Game
where

import Player
import Team

import System.Random
import Control.Monad.IO.Class

data TeamInGame = TeamInGame
  { team :: Team
  , score :: Int
  , inPossession :: Bool
  }

data Game = Game
  { homeTeam :: Team
  , awayTeam :: Team
  , homeTeamScore :: Int
  , awayTeamScore :: Int
  , teamInPossession :: Team
  , points :: [Point]
  }

winner :: Game -> Team
winner g
  | homeTeamScore g > awayTeamScore g = homeTeam g
  | otherwise = awayTeam g

loser :: Game -> Team
loser g
  | awayTeamScore g >= homeTeamScore g = homeTeam g
  | otherwise = awayTeam g

otherTeam :: Game -> Team
otherTeam g
  | (teamInPossession g) == (homeTeam g) = homeTeam g
  | otherwise = awayTeam g

winningScore :: Game -> Int
winningScore g
  | winner g == homeTeam g = homeTeamScore g
  | otherwise  = awayTeamScore g

losingScore :: Game -> Int
losingScore g
  | loser g == homeTeam g = homeTeamScore g
  | otherwise = awayTeamScore g
  
  

--data GameResults = GameResults
  --{ winner :: Team
  --, loser :: Team
  --, winningScore :: Int
  --, losingScore :: Int
  --}

data Point = Point
  { scoringTeam :: Team
  , assist :: Player
  , goal :: Player
  , turnoversBy :: [Player]
  , blocksBy :: [Player]
  , homeTeamPoints :: Int
  , awayTeamPoints :: Int
  }


instance Show Game where
  show g = do
    "Team: " ++ (teamName $ winner g) ++ " beat " ++ (teamName $ loser g) ++ " with a score of " ++ (show $ winningScore g) ++ " - " ++ (show $ losingScore g)


removePlayer _ [] = []
removePlayer player (p:ps) | player == p = removePlayer player ps
                           | otherwise = p : removePlayer player ps

randomOtherPlayer player ps = randomPlayer (removePlayer player ps)

randomPlayer ps = do
  g <- getStdGen
  return $ ps !! (head (randomRs (0,(length ps) - 1) g))


playPoint game = do
  turnover  <- randomIO :: IO Bool
  putStrLn $ show turnover
  let scoring = if turnover == True
                then otherTeam game
                else teamInPossession game
  let homeTeamPts = if scoring == (homeTeam game)
                      then (homeTeamScore game) + 1
                      else homeTeamScore game
  let awayTeamPts = if scoring == (awayTeam game)
                      then (awayTeamScore game) + 1
                      else awayTeamScore game

                      
  assister <- randomPlayer $ players scoring
  scorer <- randomOtherPlayer assister $ players scoring
  let newPoint = Point
                   { scoringTeam = scoring
                   , assist = assister
                   , goal = scorer
                   , homeTeamPoints = homeTeamPts
                   , awayTeamPoints = awayTeamPts
                   , turnoversBy = []
                   , blocksBy = []
                   }
  return $ Game { homeTeam = homeTeam game
                , awayTeam = awayTeam game
                , homeTeamScore = homeTeamPts
                , awayTeamScore = awayTeamPts
                , teamInPossession = otherTeam game
                , points = (points game) ++ [newPoint]
  }


--generateIncidentName :: HandlerFor App [Char]
--generateWinnner team1 team2 = do
  --g <- getStdGen
  --scoringPlayers (randoms g :: [Player])
  --return $ "incident_" ++ take 10 (randomRs ('a', 'z') g)

-- TODO - generate random points - this should be a game generator
play :: Game -> IO (Game)
play game = do
  g <- playPoint game
  g1 <- playPoint g
  --p <- randomPlayer $ team1 game
  return g1

  --, points = randomPoints team1 team2

