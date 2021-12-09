{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Codec.Picture.Extra
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A collection of functions to scale, crop, flip images with JuicyPixels.
module Codec.Picture.Extra
  ( -- * Scaling
    scaleBilinear,

    -- * Cropping
    crop,

    -- * Rotation
    flipHorizontally,
    flipVertically,
    rotateLeft90,
    rotateRight90,
    rotate180,

    -- * Other
    beside,
    below,
  )
where

import Codec.Picture
import qualified Codec.Picture.Types as M
import Control.Monad.ST
import Data.List (foldl1')

-- | Scale an image using bi-linear interpolation.
scaleBilinear ::
  ( Pixel a,
    Bounded (PixelBaseComponent a),
    Integral (PixelBaseComponent a)
  ) =>
  -- | Desired width
  Int ->
  -- | Desired height
  Int ->
  -- | Original image
  Image a ->
  -- | Scaled image
  Image a
scaleBilinear width height img@Image {..}
  | width <= 0 || height <= 0 =
      generateImage (error "scaleBilinear: absurd") (max 0 width) (max 0 height)
  | otherwise = runST $ do
      mimg <- M.newMutableImage width height
      let sx, sy :: Float
          sx = fromIntegral imageWidth / fromIntegral width
          sy = fromIntegral imageHeight / fromIntegral height
          go x' y'
            | x' >= width = go 0 (y' + 1)
            | y' >= height = M.unsafeFreezeImage mimg
            | otherwise = do
                let xf = fromIntegral x' * sx
                    yf = fromIntegral y' * sy
                    x, y :: Int
                    x = floor xf
                    y = floor yf
                    δx = xf - fromIntegral x
                    δy = yf - fromIntegral y
                    pixelAt' i j =
                      pixelAt img (min (pred imageWidth) i) (min (pred imageHeight) j)
                writePixel mimg x' y' $
                  mulp (pixelAt' x y) ((1 - δx) * (1 - δy))
                    `addp` mulp (pixelAt' (x + 1) y) (δx * (1 - δy))
                    `addp` mulp (pixelAt' x (y + 1)) ((1 - δx) * δy)
                    `addp` mulp (pixelAt' (x + 1) (y + 1)) (δx * δy)
                go (x' + 1) y'
      go 0 0
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelRGBA16 -> Image M.PixelRGBA16 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelRGBA8 -> Image M.PixelRGBA8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelCMYK16 -> Image M.PixelCMYK16 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelCMYK8 -> Image M.PixelCMYK8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelYCbCr8 -> Image M.PixelYCbCr8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelRGB16 -> Image M.PixelRGB16 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelYCbCrK8 -> Image M.PixelYCbCrK8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelRGB8 -> Image M.PixelRGB8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelYA16 -> Image M.PixelYA16 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.PixelYA8 -> Image M.PixelYA8 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.Pixel32 -> Image M.Pixel32 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.Pixel16 -> Image M.Pixel16 #-}
{-# SPECIALIZE scaleBilinear :: Int -> Int -> Image M.Pixel8 -> Image M.Pixel8 #-}

mulp :: (Pixel a, Integral (PixelBaseComponent a)) => a -> Float -> a
mulp pixel x = colorMap (floor . (* x) . fromIntegral) pixel
{-# INLINE mulp #-}

addp ::
  forall a.
  ( Pixel a,
    Bounded (PixelBaseComponent a),
    Integral (PixelBaseComponent a)
  ) =>
  a ->
  a ->
  a
addp = mixWith (const f)
  where
    f x y =
      fromIntegral $
        (maxBound :: PixelBaseComponent a) `min` (fromIntegral x + fromIntegral y)
{-# INLINE addp #-}

-- | Crop an image. If the supplied coordinates are greater than the size of
-- the image, the image boundaries are used instead.
crop ::
  Pixel a =>
  -- | Index (X axis) of first pixel to include
  Int ->
  -- | Index (Y axis) of first pixel to include
  Int ->
  -- | Width of resulting image
  Int ->
  -- | Height of resulting image
  Int ->
  -- | Original image
  Image a ->
  -- | Cropped image
  Image a
crop x' y' w' h' img@Image {..} =
  generateImage gen w h
  where
    gen i j = pixelAt img (x + i) (y + j)
    x = min (imageWidth - 1) x'
    y = min (imageHeight - 1) y'
    w = min (imageWidth - x) w'
    h = min (imageHeight - y) h'
{-# INLINEABLE crop #-}

-- | Flip an image horizontally.
flipHorizontally :: Pixel a => Image a -> Image a
flipHorizontally img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x = pixelAt img (imageWidth - 1 - x)
{-# INLINEABLE flipHorizontally #-}

-- | Flip an image vertically.
flipVertically :: Pixel a => Image a -> Image a
flipVertically img@Image {..} =
  generateImage gen imageWidth imageHeight
  where
    gen x y = pixelAt img x (imageHeight - 1 - y)
{-# INLINEABLE flipVertically #-}

-- | Rotate an image to the left by 90°.
rotateLeft90 :: Pixel a => Image a -> Image a
rotateLeft90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img (imageWidth - 1 - y) x
{-# INLINEABLE rotateLeft90 #-}

-- | Rotate an image to the right by 90°.
rotateRight90 :: Pixel a => Image a -> Image a
rotateRight90 img@Image {..} =
  generateImage gen imageHeight imageWidth
  where
    gen x y = pixelAt img y (imageHeight - 1 - x)
{-# INLINEABLE rotateRight90 #-}

-- | Rotate an image by 180°, i.e flip both vertically and horizontally.
--
-- @since 0.2.0
rotate180 :: Pixel a => Image a -> Image a
rotate180 img@(Image w h _) = generateImage g w h
  where
    g x y = pixelAt img (w - 1 - x) (h - 1 - y)
{-# INLINEABLE rotate180 #-}

-- | Create an image by placing several images side by side. If the images
-- are of differnet heights the smallest height is used.
--
-- @since 0.2.0
beside :: Pixel a => [Image a] -> Image a
beside = foldl1' go
  where
    go :: Pixel a => Image a -> Image a -> Image a
    go img1@(Image w1 h1 _) img2@(Image w2 h2 _) =
      generateImage g (w1 + w2) h
      where
        g x
          | x < w1 = pixelAt img1 x
          | otherwise = pixelAt img2 (x - w1)
        h = min h1 h2
{-# INLINEABLE beside #-}

-- | Create an image by placing several images in a vertical stack. If the
-- images are of differnet widths the smallest width is used.
--
-- @since 0.2.0
below :: Pixel a => [Image a] -> Image a
below = foldl1' go
  where
    go :: Pixel a => Image a -> Image a -> Image a
    go img1@(Image w1 h1 _) img2@(Image w2 h2 _) =
      generateImage g w (h1 + h2)
      where
        g x y
          | y < h1 = pixelAt img1 x y
          | otherwise = pixelAt img2 x (y - h1)
        w = min w1 w2
{-# INLINEABLE below #-}
