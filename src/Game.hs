module Game
where

import Player
import Team
import Point

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

instance Show Game where
  show g = do
    (show $ points g) ++ "\n\nTeam: " ++ (teamName $ winner g) ++ " beat " ++ (teamName $ loser g) ++ " with a score of " ++ (show $ winningScore g) ++ " - " ++ (show $ losingScore g) ++ "\n"

winningTeam :: Game -> Int -> Maybe Team
winningTeam g s
  | homeTeamScore g > awayTeamScore g && homeTeamScore g >= s = Just $ homeTeam g
  | awayTeamScore g > homeTeamScore g && awayTeamScore g >= s = Just $ awayTeam g
  | otherwise = Nothing


winner :: Game -> Team
winner g
  | homeTeamScore g > awayTeamScore g = homeTeam g
  | otherwise = awayTeam g

loser :: Game -> Team
loser g
  | awayTeamScore g >= homeTeamScore g = homeTeam g
  | otherwise = awayTeam g

otherTeam :: Game -> Team -> Team
otherTeam g t
  | homeTeam g == t = awayTeam g
  | otherwise = homeTeam g

teamNotInPossession :: Game -> Team
teamNotInPossession g
  | (teamInPossession g) == (homeTeam g) = awayTeam g
  | otherwise = homeTeam g

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

removePlayer _ [] = []
removePlayer player (p:ps) | player == p = removePlayer player ps
                           | otherwise = p : removePlayer player ps

randomOtherPlayer player ps = randomPlayer (removePlayer player ps)

randomPlayer ps = do
  g <- getStdGen
  return $ ps !! (head (randomRs (0,(length ps) - 1) g))


playPoint :: IO Game -> IO Game
playPoint ioGame = do
  game <- ioGame
  turnover  <- randomIO :: IO Bool
  let scoring = if turnover == True
                then teamNotInPossession game
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
  let p = points game
  return $ Game { homeTeam = homeTeam game
                , awayTeam = awayTeam game
                , homeTeamScore = homeTeamPts
                , awayTeamScore = awayTeamPts
                , teamInPossession = teamNotInPossession game
                , points = p ++ [newPoint]
  }


--generateIncidentName :: HandlerFor App [Char]
--generateWinnner team1 team2 = do
  --g <- getStdGen
  --scoringPlayers (randoms g :: [Player])
  --return $ "incident_" ++ take 10 (randomRs ('a', 'z') g)

-- TODO - generate random points - this should be a game generator
playUntil :: Int -> IO Game -> IO Game
playUntil score game = do
  g <- game
  if (winningTeam g score) /= Nothing
  then return g
  else playUntil score $ playPoint $ return g

play :: Game -> IO (Game)
play game = do
  --games <- (sequence (take 6 (iterate playPoint $ return game)))
  --let playedGames = untilWinner 3 games
  endGame <- playUntil 15 $ return game
  --putStrLn $ show (games !! 1)
  --putStrLn $ show (games !! 2)
  --putStrLn $ show (games !! 3)
  --putStrLn $ show (games !! 4)
  --putStrLn $ show (games !! 5)
  --putStrLn $ show (games !! 6)
  --putStrLn $ show (games !! 7)
  --p <- randomPlayer $ team1 game
  return $ endGame
  where
    untilWinner :: Int -> [Game] -> [Game]
    untilWinner _ [] = []
    untilWinner s (x:xs) = do
      if winningTeam x s == Nothing
      then x : (untilWinner s xs)
      else [x]
      
  --, points = randomPoints team1 team2

