
module Game where

import Miso.Lens
import Miso.Lens.TH

import World

data Game = Game
  { _gameWorld :: World
  , _gameCurrentPos :: Position
  } deriving (Eq)

makeLenses ''Game


