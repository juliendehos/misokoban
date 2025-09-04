
-- read and compute the average pixel value of each png image given in the
-- arguments
--
-- "cabal run mean-asset assets/*"

import Control.Monad (forM_)
import Data.Massiv.Array hiding (forM_)
import System.Environment

import ImageProcessing

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: <image...>"
    fps -> forM_ fps $ \fp -> do
      putStrLn fp 
      img <- loadAvg32 fp
      putStrLn $ "  " <> show (img ! Ix2 0 0)

