
module Game
  ( World.assetSize
  , World.Cell(..)
  , Move(..)
  , Game
  , mkGame
  , playMove
  , forGame
  , getNiNj
  , getPlayer
  , getBoxes12
  , isRunning
  , isTerminated
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
  , _gamePlayer :: Position
  , _gameBoxes :: S.Set Position
  , _gameBoxes12 :: ([Position], [Position])
  , _gameTerminated :: Bool
  } deriving (Eq)

makeLenses ''Game

mkGame :: Int -> Game
mkGame n = Game w (w ^. worldPlayer) bs bs12 term
  where
    k = mod (n-1) (length allWorlds)
    w = allWorlds !! k
    bs = w ^. worldBoxes
    bs12 = computeBoxes12 bs w
    term = computeTerminated bs12

playMove :: Move -> Game -> Maybe Game
playMove m g =
  case m of
    MoveUp    -> tryMove g (-1,  0)
    MoveDown  -> tryMove g ( 1,  0)
    MoveLeft  -> tryMove g ( 0, -1)
    MoveRight -> tryMove g ( 0,  1)

getNiNj :: Game -> (Int, Int)
getNiNj g = g ^. gameWorld . worldNiNj

getPlayer :: Game -> Position
getPlayer g = g ^. gamePlayer

forGame :: (Monad m) => Game -> ((Int, Int) -> Cell -> m ()) -> m ()
forGame g f = 
  let nj = g ^. gameWorld . worldNiNj . _2
      b = g ^. gameWorld . worldBoard
  in V.iforM_ b $ \k c -> f (k2ij nj k) c

getBoxes12 :: Game -> ([Position], [Position])
getBoxes12 = _gameBoxes12

isRunning :: Game -> Bool
isRunning = not . _gameTerminated

isTerminated :: Game -> Bool
isTerminated = _gameTerminated

-------------------------------------------------------------------------------
-- internal
-------------------------------------------------------------------------------

computeBoxes12 :: S.Set Position -> World -> ([Position], [Position])
computeBoxes12 bs w = foldr f ([], []) bs
  where
    ij2k' = ij2k (w ^. worldNiNj . _2)
    b = w ^. worldBoard
    f ij (bs1, bs2) = case b V.! ij2k' ij of
      CellE -> (ij : bs1, bs2)
      CellT -> (bs1, ij : bs2)
      _ -> (bs1, bs2)

computeTerminated :: ([Position], [Position]) -> Bool
computeTerminated = null . fst

tryMove :: Game -> (Int, Int) -> Maybe Game
tryMove g (di, dj) 
  | isTerminated g =   -- not running
      Nothing
  | isBox1 && not isBox2 && cell2 /= CellW =    -- push a box 
      Just $ g & gamePlayer .~ ij1
               & gameBoxes .~ bs'
               & gameBoxes12 .~ bs12'
               & gameTerminated .~ computeTerminated bs12'
  | cell1 /= CellW && not isBox1 =    -- move, no push
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
    bs' = g ^. gameBoxes & S.insert ij2 . S.delete ij1
    bs12' = computeBoxes12 bs' (g ^. gameWorld)

