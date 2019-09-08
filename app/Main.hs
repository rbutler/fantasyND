{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Monoid
import System.Environment
import Web.Twitter.Conduit hiding (map)

import Lib
import Player
import Team
import Game
import Tweet

main :: IO ()
main = do
  twInfo <- getTWInfoFromEnv
  mgr <- newManager tlsManagerSettings
  let game = Game skurtzTeam butlerTeam 0 0 skurtzTeam []
  endGame <- play game
  res <- sendTweet twInfo mgr $ finalGameStr endGame
  print res
  putStrLn $ show endGame

skurtzTeam = Team
  { teamName = "Skurtz"
  , players = skurtzPlayers
  , teamScore = Nothing
  }

butlerTeam = Team
  { teamName = "Butler"
  , players = butlerPlayers
  , teamScore = Nothing
  }

butlerPlayers :: [Player]
butlerPlayers = [vasili, butler]


skurtzPlayers :: [Player]
skurtzPlayers = [skurtz, sheehan]

butler :: Player
butler = Player
 { playerName = "Butler"
 , ability = 2
 , hand = RightHand
 , position = Handler
 , goodComment = "Butler out-slutted them again throwing 8 consecutive scoobers to break the cup"
 , badComment = "We thought you knew better than to play Butler in the first game of whitesmoke"
 , excuse = "Looking for car bombs at the airport"
 , chanceToMiss = 7.3
 }

skurtz :: Player
skurtz = Player
 { playerName = "Skurtz"
 , ability = 2
 , hand = RightHand
 , position = Cutter
 , goodComment = "Skurtz caught whatever you threw at him, even lammers, with his eyes closed"
 , badComment = "You cut deep for Skurtz' air bounce huck"
 , excuse = "finishing Sheehan's homework"
 , chanceToMiss = 1.5
 }


sheehan :: Player
sheehan = Player
 { playerName = "Sheehan"
 , ability = 2
 , hand = RightHand
 , position = Cutter
 , goodComment = "Sheehan had 8 layout Ds this game"
 , badComment = "Sheehan puked bile for the first half of the game"
 , excuse = "Drove to Ohio"
 , chanceToMiss = 9.7
 }

vasili :: Player
vasili = Player
 { playerName = "Vasili"
 , ability = 2
 , hand = LeftHand
 , position = Handler
 , goodComment = "Riding high after sweet talking Dave Brown into allowing your team to play today"
 , badComment = "Opposing team was not blind. - 5 points due to excessive travel calls"
 , excuse = "still in Dave Brown's office trying to get the club reinstated"
 , chanceToMiss = 3.3
 }

