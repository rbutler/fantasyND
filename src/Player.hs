module Player
where

data Position = Handler | Cutter
  deriving (Show, Eq, Enum)

data Hand = LeftHand | RightHand
  deriving (Show, Eq, Enum)

data Player = Player
  { playerName :: String
  , ability :: Int
  , hand :: Hand
  , position :: Position
  , goodComment :: String
  , badComment :: String
  , excuse :: String
  , chanceToMiss :: Float
  }
  deriving (Show, Eq)
