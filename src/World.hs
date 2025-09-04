
module World where

import Control.Lens
import Data.Set qualified as S
import Data.Vector qualified as V

-------------------------------------------------------------------------------
--  params
-------------------------------------------------------------------------------

assetSize :: Int
assetSize = 32

-------------------------------------------------------------------------------
--  types
-------------------------------------------------------------------------------

data Cell
  = CellE   -- Empty
  | CellT   -- Target
  | CellW   -- Wall
  deriving (Eq, Show)

type Position = (Int, Int)

data World = World
  { _worldNiNj :: (Int, Int)
  , _worldBoard :: V.Vector Cell
  , _worldBoxes :: S.Set Position
  , _worldInitialPos :: Position
  } deriving (Eq, Show)

makeLenses ''World

-------------------------------------------------------------------------------
-- MyShow typeclass, for generating worlds source code from images
-------------------------------------------------------------------------------

class Show a => MyShow a where
  myshow :: a -> String
  myshow = show

instance MyShow (Int, Int)

instance MyShow Cell

instance MyShow (S.Set Position) where
  myshow x = "S." ++ show x

instance MyShow (V.Vector Cell) where
  myshow x = "V.fromList " ++ show x

instance MyShow World where
  myshow (World ninj board boxes initialPos)
    = "World "
    ++ "{ _worldNiNj = " ++ myshow ninj 
    ++ ", _worldBoard = " ++ myshow board
    ++ ", _worldBoxes = " ++ myshow boxes
    ++ ", _worldInitialPos = " ++ myshow initialPos
    ++ "}"

