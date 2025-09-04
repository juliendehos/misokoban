
module Model where

import Miso.Lens
import Miso.Lens.TH

import Game

data Model = Model
  { _modelGame :: Game
  , _modelNbMoves :: Int
  , _modelStarted :: Bool
  } deriving (Eq)

makeLenses ''Model

initialModel :: Model
initialModel = Model (mkGame 1) 0 False

mkModel :: Int -> Model
mkModel n = Model (mkGame n) 0 True

