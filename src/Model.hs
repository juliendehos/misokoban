
module Model where

-- import Miso
import Miso.Lens
import Miso.Lens.TH

import Game
-- import World

newtype Model = Model
  { _modelPos :: (Int, Int)
  } deriving (Eq)

makeLenses ''Model

mkModel :: Model
mkModel = Model (0, 0)


