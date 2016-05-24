{-# LANGUAGE RecordWildCards #-}

module Codec.Picture.Extra
  ( -- * Scaling
    scaleBicubic
    -- * Cropping
  , crop
    -- * Rotation
  , flipHorizontally
  , flipVertically
  , rotateLeft90
  , rotateRight90 )
where

import Codec.Picture
-- import Codec.Picture.Types
-- import Control.Monad.ST

-- | Scale image using bi-cubic interpolation.

scaleBicubic :: Pixel a => Int -> Int -> Image a -> Image a
scaleBicubic = undefined -- TODO

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
flipHorizontally = undefined -- TODO

-- | Flip image vertically.

flipVertically :: Pixel a => Image a -> Image s
flipVertically = undefined -- TODO

-- | Rotate image to the left by 90°.

rotateLeft90 :: Pixel a => Image a -> Image a
rotateLeft90 = undefined -- TODO

-- | Rotate image to the right by 90°.

rotateRight90 :: Pixel a => Image a -> Image a
rotateRight90 = undefined -- TODO
