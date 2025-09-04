{-# LANGUAGE ScopedTypeVariables #-}

module ImageProcessing where

import Data.Function ((&))
import Data.Massiv.Array as M hiding (mapM_)
import Data.Massiv.Array.IO
import Graphics.Color.Model (RGB)

import World

type MyPixel = Pixel (Alpha RGB) Double

loadAvg32 :: FilePath -> IO (Array S Ix2 MyPixel)
loadAvg32 fp = do
  img :: Image S (Alpha RGB) Word8 <- readImage fp
  pure $ img
    & M.map toPixelD 
    & computeAs S 
    & mapStencil Continue (avgStencil $ Sz2 assetSize assetSize) 
    & computeWithStrideAs S (Stride $ Ix2 assetSize assetSize) 

