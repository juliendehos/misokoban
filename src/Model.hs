
module Model where

import Miso.Lens
import Miso.Lens.TH

import Game

data Model = Model
  { _modelGame :: Game
  , _modelNbMoves :: Int
  , _modelLevel :: Int    -- 1-indexed world, in allWorlds
  , _modelTime :: Double
  } deriving (Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model (mkGame 1) 0 1 0

mkModel :: Int -> Model
mkModel n = Model (mkGame n) 0 n 0

