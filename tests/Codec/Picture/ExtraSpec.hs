module Codec.Picture.ExtraSpec
  ( main,
    spec,
  )
where

import Codec.Picture
import Codec.Picture.Extra
import Control.Monad
import Data.Function (on)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scaleBilinear" scaleBilinearSpec
  describe "crop" cropSpec
  describe "flipHorizontally" flipHorizontallySpec
  describe "flipVertically" flipVerticallySpec
  describe "rotateLeft90Spec" rotateLeft90Spec
  describe "rotateRight90Spec" rotateRight90Spec
  describe "rotate180" rotate180Spec
  describe "beside" besideSpec
  describe "below" belowSpec

scaleBilinearSpec :: Spec
scaleBilinearSpec = do
  context "when called with orginial dimensions" $
    it "produces the same image" $
      checkWithFiles
        (scaleBilinear 512 512)
        "data-examples/macaque.png"
        "data-examples/macaque.png"
  context "when we scale down" $
    it "produces correct image" $
      checkWithFiles
        (scaleBilinear 100 100)
        "data-examples/macaque.png"
        "data-examples/macaque-scaled-down.png"
  context "when we scale up" $
    it "produces correct image" $
      checkWithFiles
        (scaleBilinear 600 600)
        "data-examples/macaque.png"
        "data-examples/macaque-scaled-up.png"

cropSpec :: Spec
cropSpec = do
  context "when we pass arguments within image size (square)" $
    it "produces correct image" $
      checkWithFiles
        (crop 211 210 178 191)
        "data-examples/macaque.png"
        "data-examples/macaque-cropped.png"
  context "when we pass arguments within image size (vertical)" $
    it "produces correct image" $
      checkWithFiles
        (crop 0 512 512 512)
        "data-examples/macaque-below.png"
        "data-examples/macaque.png"

flipHorizontallySpec :: Spec
flipHorizontallySpec =
  context "when we flip horizontally" $
    it "produces correct image" $
      checkWithFiles
        flipHorizontally
        "data-examples/macaque.png"
        "data-examples/macaque-horizontal-flip.png"

flipVerticallySpec :: Spec
flipVerticallySpec =
  context "when we flip vertically" $
    it "produces correct image" $
      checkWithFiles
        flipVertically
        "data-examples/macaque.png"
        "data-examples/macaque-vertical-flip.png"

rotateLeft90Spec :: Spec
rotateLeft90Spec =
  context "when we rotate to the left by 90°" $
    it "produces correct image" $
      checkWithFiles
        rotateLeft90
        "data-examples/macaque.png"
        "data-examples/macaque-left-rotated.png"

rotateRight90Spec :: Spec
rotateRight90Spec =
  context "when we rotate to the right by 90°" $
    it "produces correct image" $
      checkWithFiles
        rotateRight90
        "data-examples/macaque.png"
        "data-examples/macaque-right-rotated.png"

rotate180Spec :: Spec
rotate180Spec =
  context "when we rotate by 180°" $
    it "produces correct image" $
      checkWithFiles
        rotate180
        "data-examples/macaque.png"
        "data-examples/macaque-180-rotated.png"

besideSpec :: Spec
besideSpec =
  context "when we place images beside each other" $
    it "produces correct image" $
      checkWithFiles
        (\x -> beside [x, x])
        "data-examples/macaque.png"
        "data-examples/macaque-beside.png"

belowSpec :: Spec
belowSpec =
  context "when we place images below each other" $
    it "produces correct image" $
      checkWithFiles
        (\x -> below [x, x])
        "data-examples/macaque.png"
        "data-examples/macaque-below.png"

-- | Run a transforming on the image loaded from a file and compare the
-- resulting image with the contents of another file.
checkWithFiles ::
  -- | Transformation to test
  (Image PixelRGB8 -> Image PixelRGB8) ->
  -- | Where to get the original image
  FilePath ->
  -- | Where to get the image to compare with
  FilePath ->
  Expectation
checkWithFiles f opath fpath = do
  (Right (ImageRGB8 original)) <- readImage opath
  (Right (ImageRGB8 result)) <- readImage fpath
  f original `blindlySatisfy` sameImage result

-- | The same as 'shouldSatisfy', but doesn't care if its argument is an
-- instance of 'Show' or not.
blindlySatisfy :: a -> (a -> Bool) -> Expectation
v `blindlySatisfy` p =
  unless (p v) (expectationFailure "predicate failed")

-- | The equality test for images.
sameImage :: Image PixelRGB8 -> Image PixelRGB8 -> Bool
sameImage a b =
  ((==) `on` imageWidth) a b
    && ((==) `on` imageHeight) a b
    && ((==) `on` imageData) a b
