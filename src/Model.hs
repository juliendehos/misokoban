
module Model where

-- import Miso
import Miso.Lens
import Miso.Lens.TH

import Game

data Model = Model
  { _modelGame :: Game
  , _modelNbMoves :: Int
  } deriving (Eq)

makeLenses ''Model

mkModel :: Int -> Model
mkModel n = Model (mkGame n) 0

