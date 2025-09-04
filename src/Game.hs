
module Game
  ( World.assetSize
  , World.Cell(..)
  , Move(..)
  , Game(..)
  , mkGame
  , playMove
  , forGame
  , getNiNj
  , getBoxes12
  ) where

import Control.Lens
import Data.Set qualified as S
import Data.Vector qualified as V

import GeneratedWorlds
import World

data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight

data Game = Game
  { _gameWorld :: World
  , _gameWorldIdx :: Int    -- 1-indexed world, in allWorlds
  , _gameCurrentPos :: Position
  , _gameCurrentBoxes :: S.Set Position
  } deriving (Eq)

makeLenses ''Game

mkGame :: Int -> Game
mkGame n = Game w (k + 1) (w ^. worldPlayer) (w ^. worldBoxes)
  where
    k = n-1 `mod` length allWorlds
    w = allWorlds !! k

playMove :: Move -> Game -> Game
playMove m g =
  case m of
    MoveUp    -> g & gameCurrentPos . _1 -~ 1
    MoveDown  -> g & gameCurrentPos . _1 +~ 1
    MoveLeft  -> g & gameCurrentPos . _2 -~ 1
    MoveRight -> g & gameCurrentPos . _2 +~ 1

getNiNj :: Game -> (Int, Int)
getNiNj g = g ^. gameWorld . worldNiNj

forGame :: (Monad m) => Game -> ((Int, Int) -> Cell -> m ()) -> m ()
forGame g f = 
  let nj = g ^. gameWorld . worldNiNj . _2
      b = g ^. gameWorld . worldBoard
  in V.iforM_ b $ \k c -> f (k2ij nj k) c

getBoxes12 :: Game -> ([Position], [Position])
getBoxes12 g = foldr f ([], []) (g ^. gameCurrentBoxes)
  where
    ij2k' = ij2k (g ^. gameWorld . worldNiNj . _2)
    b = g ^. gameWorld . worldBoard
    f ij (bs1, bs2) = case b V.! ij2k' ij of
      CellE -> (ij : bs1, bs2)
      CellT -> (bs1, ij : bs2)
      _ -> (bs1, bs2)

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

k2ij :: Int -> Int -> (Int, Int)
k2ij nj k = (k `div` nj, k`rem` nj)

