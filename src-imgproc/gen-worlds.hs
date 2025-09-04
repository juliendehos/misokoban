
-- read png images (screenshots of sokoban maps) and generate the corresponding
-- haskell code, to be used for building the miso app
-- 
-- cabal run gen-worlds
-- cp GeneratedWorlds.hs src/

{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (forM)
import Data.List (intercalate, sort)
import Data.Massiv.Array qualified as M
import Data.Set qualified as S
import Data.Tuple (swap)
import Data.Vector qualified as V
import Graphics.Pixel (Pixel(PixelRGBA))
import Miso.Lens
import System.Directory (listDirectory)
import System.IO (withFile, hPutStr, hPutStrLn, IOMode(WriteMode))

import ImageProcessing
import World

srcDir :: FilePath
srcDir = "worlds/"

dstPath :: FilePath
dstPath = "GeneratedWorlds.hs"

data CellType
  = CtEmpty
  | CtWall
  | CtTarget
  | CtBox1
  | CtBox2
  | CtPlayer
  deriving (Eq, Ord)

ctDict :: [(CellType, MyPixel)]
ctDict = 
  [ (CtBox1,   PixelRGBA 0.713 0.525 0.260 1.0)
  , (CtBox2,   PixelRGBA 0.534 0.328 0.137 1.0)
  , (CtEmpty,  PixelRGBA 0.861 0.829 0.668 1.0)
  , (CtPlayer, PixelRGBA 0.627 0.638 0.546 1.0)
  , (CtTarget, PixelRGBA 0.859 0.807 0.656 1.0)
  , (CtWall,   PixelRGBA 0.400 0.400 0.400 1.0)
  ]

distPx :: MyPixel -> MyPixel -> Double
distPx p1 p2 =
  let (PixelRGBA dr dg db _) = p1 - p2
  in dr*dr + dg*dg + db*db

findBestCellType :: MyPixel -> CellType
findBestCellType px0 =
  snd $ minimum $ map (swap . fmap (distPx px0)) ctDict

imageToWorld :: FilePath -> IO World
imageToWorld fp = do
  putStrLn fp 
  img <- loadAvg32 fp
  let 
    M.Sz2 ni nj = M.size img
    f w (M.Ix2 i j) px = 
      case findBestCellType px of
        CtWall    -> w & worldBoard %~ (V.// [(ij2k nj (i, j), CellW)])
        CtTarget  -> w & worldBoard %~ (V.// [(ij2k nj (i, j), CellT)])
        CtBox1    -> w & worldBoxes %~ S.insert (i, j)
        CtBox2    -> w & worldBoxes %~ S.insert (i, j)
                       & worldBoard %~ (V.// [(ij2k nj (i, j), CellT)])
        CtPlayer  -> w & worldInitialPos .~ (i, j)
        CtEmpty   -> w

  pure $ M.ifoldlS f (mkWorld (ni, nj)) img

ij2k :: Int -> (Int, Int) -> Int
ij2k nj (i, j) = i*nj + j

main :: IO ()
main = do
  paths <- fmap (srcDir <>) . sort <$> listDirectory srcDir
  worlds <- forM paths imageToWorld
  withFile dstPath WriteMode $ \h -> do
    hPutStrLn h "module GeneratedWorlds where\n"
    hPutStrLn h "import Data.Set qualified as S"
    hPutStrLn h "import Data.Vector qualified as V\n"
    hPutStrLn h "import World\n"
    hPutStrLn h "allWorlds :: [World]"
    hPutStr h "allWorlds = \n  [ "
    hPutStrLn h $ intercalate "\n  , " $ map myshow worlds
    hPutStrLn h "  ]\n"

