
module Game
  ( World.assetSize
  , World.Cell(..)
  , Move(..)
  , Game(..)
  , mkGame
  , playMove
  , forGame
  , getNiNj
  , computeBoxes12
  , computeRunning
  , computeTerminated
  , getLevel
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
  , _gamePlayer :: Position
  , _gameBoxes :: S.Set Position
  } deriving (Eq)

makeLenses ''Game

getLevel :: Game -> Int
getLevel = _gameWorldIdx

mkGame :: Int -> Game
mkGame n = Game w (k + 1) (w ^. worldPlayer) (w ^. worldBoxes)
  where
    k = mod (n-1) (length allWorlds)
    w = allWorlds !! k

playMove :: Move -> Game -> Maybe Game
playMove m g =
  case m of
    MoveUp    -> tryMove g (-1,  0)
    MoveDown  -> tryMove g ( 1,  0)
    MoveLeft  -> tryMove g ( 0, -1)
    MoveRight -> tryMove g ( 0,  1)

getNiNj :: Game -> (Int, Int)
getNiNj g = g ^. gameWorld . worldNiNj

forGame :: (Monad m) => Game -> ((Int, Int) -> Cell -> m ()) -> m ()
forGame g f = 
  let nj = g ^. gameWorld . worldNiNj . _2
      b = g ^. gameWorld . worldBoard
  in V.iforM_ b $ \k c -> f (k2ij nj k) c

computeBoxes12 :: Game -> ([Position], [Position])
computeBoxes12 g = foldr f ([], []) (g ^. gameBoxes)
  where
    ij2k' = ij2k (g ^. gameWorld . worldNiNj . _2)
    b = g ^. gameWorld . worldBoard
    f ij (bs1, bs2) = case b V.! ij2k' ij of
      CellE -> (ij : bs1, bs2)
      CellT -> (bs1, ij : bs2)
      _ -> (bs1, bs2)

computeTerminated :: Game -> Bool
computeTerminated = null . fst . computeBoxes12

computeRunning :: Game -> Bool
computeRunning = not . computeTerminated

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

tryMove :: Game -> (Int, Int) -> Maybe Game
tryMove g (di, dj) 
  | computeTerminated g = 
      Nothing
  | isBox1 && not isBox2 && cell2 /= CellW = 
      Just $ g & gamePlayer .~ ij1
               & gameBoxes %~ S.insert ij2 . S.delete ij1
  | cell1 /= CellW && not isBox1 = 
      Just $ g & gamePlayer .~ ij1
  | otherwise = 
      Nothing
  where
    nj = g ^. gameWorld . worldNiNj . _2
    (i0, j0) = g ^. gamePlayer
    ij1@(i1, j1) = (i0+di, j0+dj)
    ij2 = (i1+di, j1+dj)
    cell1 = (g ^. gameWorld . worldBoard) V.! ij2k nj ij1
    cell2 = (g ^. gameWorld . worldBoard) V.! ij2k nj ij2
    isBox1 = ij1 `S.member` (g ^. gameBoxes)
    isBox2 = ij2 `S.member` (g ^. gameBoxes)

