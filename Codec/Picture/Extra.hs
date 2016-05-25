-- |
-- Module      :  Codec.Picture.Extra
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utilities for image transformation with JuicyPixels.

{-# LANGUAGE RecordWildCards #-}

module Codec.Picture.Extra
  ( -- * Scaling
    scaleBilinear
    -- * Cropping
  , crop
    -- * Rotation
  , flipHorizontally
  , flipVertically
  , rotateLeft90
  , rotateRight90 )
where

import Codec.Picture
import Control.Monad.ST
import qualified Codec.Picture.Types as M

-- | Scale image using bi-linear interpolation. This is specialized to
-- 'PixelRGB8' only for speed (polymorphic version is easily written, but
-- it's more than twice as slow).

scaleBilinear
  :: Int               -- ^ Desired width
  -> Int               -- ^ Desired height
  -> Image PixelRGB8   -- ^ Original image
  -> Image PixelRGB8   -- ^ Scaled image
scaleBilinear width height img@Image {..} = runST $ do
  mimg <- M.newMutableImage width height
  let sx, sy :: Float
      sx = fromIntegral imageWidth  / fromIntegral width
      sy = fromIntegral imageHeight / fromIntegral height
      go x' y'
        | x' >= width = go 0 (y' + 1)
        | y' >= height = M.unsafeFreezeImage mimg
        | otherwise = do
            let xf = fromIntegral x' * sx
                yf = fromIntegral y' * sy
                x, y :: Int
                x  = floor xf
                y  = floor yf
                δx = xf - fromIntegral x
                δy = yf - fromIntegral y
                pixelAt' i j =
                  if i >= imageWidth || j >= imageHeight
                    then PixelRGB8 0 0 0
                    else pixelAt img i j
            writePixel mimg x' y' $
              mulp (pixelAt' x y) ((1 - δx) * (1 - δy)) `addp`
              mulp (pixelAt' (x + 1) y) (δx * (1 - δy)) `addp`
              mulp (pixelAt' x (y + 1)) ((1 - δx) * δy) `addp`
              mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
            go (x' + 1) y'
  go 0 0

mulp :: PixelRGB8 -> Float -> PixelRGB8
mulp pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp :: PixelRGB8 -> PixelRGB8 -> PixelRGB8
addp = mixWith (const f)
  where
    f x y = fromIntegral $
      (0xff :: Pixel8) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}

-- | Crop given image. If supplied coordinates are greater than size of
-- original image, image boundaries are used instead.

crop :: Pixel a
  => Int               -- ^ Index (X axis) of first pixel to include
  -> Int               -- ^ Index (Y axis) of first pixel to include
  -> Int               -- ^ Width of resulting image
  -> Int               -- ^ Height of resulting image
  -> Image a           -- ^ Original image
  -> Image a           -- ^ Cropped image
crop x' y' w' h' img@Image {..} =
  generateImage gen w h
  where
    gen i j = pixelAt img (x + i) (y + j)
    x = min (imageWidth  - 1) x'
    y = min (imageHeight - 1) y'
    w = min (imageWidth  - x) w'
    h = min (imageWidth  - y) h'

-- | Flip image horizontally.

flipHorizontally :: Pixel a => Image a -> Image a
flipHorizontally img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x = pixelAt img (imageWidth - 1 - x)

-- | Flip image vertically.

flipVertically :: Pixel a => Image a -> Image a
flipVertically img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img x (imageHeight - 1 - y)

-- | Rotate image to the left by 90°.

rotateLeft90 :: Pixel a => Image a -> Image a
rotateLeft90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img y x

-- | Rotate image to the right by 90°.

rotateRight90 :: Pixel a => Image a -> Image a
rotateRight90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img y (imageHeight - 1 - x)
