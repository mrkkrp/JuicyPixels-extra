module Codec.Picture.ExtraSpec
  ( main
  , spec )
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
  describe "crop"              cropSpec
  describe "flipHorizontally"  flipHorizontallySpec
  describe "flipVertically"    flipVerticallySpec
  describe "rotateLeft90Spec"  rotateLeft90Spec
  describe "rotateRight90Spec" rotateRight90Spec

cropSpec :: Spec
cropSpec =
  context "when we pass arguments within image size" $
    it "produces correct image" $
      checkWithFiles (crop 211 210 178 191)
        "data-examples/lenna.png"
        "data-examples/lenna-cropped.png"

flipHorizontallySpec :: Spec
flipHorizontallySpec =
  context "when we flip horizontally" $
    it "produces correct image" $
      checkWithFiles flipHorizontally
        "data-examples/lenna.png"
        "data-examples/lenna-horizontal-flip.png"

flipVerticallySpec :: Spec
flipVerticallySpec =
  context "when we flip vertically" $
    it "produces correct image" $
      checkWithFiles flipVertically
        "data-examples/lenna.png"
        "data-examples/lenna-vertical-flip.png"

rotateLeft90Spec :: Spec
rotateLeft90Spec =
  context "when we rotate to the left by 90°" $
    it "produces correct image" $
      checkWithFiles rotateLeft90
        "data-examples/lenna.png"
        "data-examples/lenna-left-rotated.png"

rotateRight90Spec :: Spec
rotateRight90Spec =
  context "when we rotate to the right by 90°" $
    it "produces correct image" $
      checkWithFiles rotateRight90
        "data-examples/lenna.png"
        "data-examples/lenna-right-rotated.png"

-- | Run given transforming function on image loaded from one file and
-- compare resulting image with contents of another file.

checkWithFiles
  :: (Image PixelRGB8 -> Image PixelRGB8) -- ^ Transformation to test
  -> FilePath          -- ^ Where to get the original image
  -> FilePath          -- ^ Where to get image to compare with
  -> Expectation
checkWithFiles f opath fpath = do
  (Right (ImageRGB8 original)) <- readImage opath
  (Right (ImageRGB8 flipped))  <- readImage fpath
  f original `blindlySatisfy` sameImage flipped

-- | The same as 'shouldSatisfy', but doesn't care if its argument is
-- instance of 'Show' or not.

blindlySatisfy :: a -> (a -> Bool) -> Expectation
v `blindlySatisfy` p =
  unless (p v) (expectationFailure "predicate failed")

-- | Equality test for images.

sameImage :: Image PixelRGB8 -> Image PixelRGB8 -> Bool
sameImage a b =
  ((==) `on` imageWidth)  a b &&
  ((==) `on` imageHeight) a b &&
  ((==) `on` imageData)   a b
