--
-- Tests for the ‘JuicyPixels-extra’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

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
  describe "scaleBilinear"     scaleBilinearSpec
  describe "crop"              cropSpec
  describe "flipHorizontally"  flipHorizontallySpec
  describe "flipVertically"    flipVerticallySpec
  describe "rotateLeft90Spec"  rotateLeft90Spec
  describe "rotateRight90Spec" rotateRight90Spec
  describe "rotate180"         rotate180Spec
  describe "beside"            besideSpec
  describe "below"             belowSpec

scaleBilinearSpec :: Spec
scaleBilinearSpec = do
  context "when called with orginial dimensions" $
    it "produces the same image" $
      checkWithFiles (scaleBilinear 512 512)
        "data-examples/lenna.png"
        "data-examples/lenna.png"
  context "when we scale down" $
    it "produces correct image" $
      checkWithFiles (scaleBilinear 100 100)
        "data-examples/lenna.png"
        "data-examples/lenna-scaled-down.png"
  context "when we scale up" $
    it "produces correct image" $
      checkWithFiles (scaleBilinear 600 600)
        "data-examples/lenna.png"
        "data-examples/lenna-scaled-up.png"

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

rotate180Spec :: Spec
rotate180Spec =
  context "when we rotate by 180°" $
    it "produces correct image" $
      checkWithFiles rotate180
        "data-examples/lenna.png"
        "data-examples/lenna-180-rotated.png"

besideSpec :: Spec
besideSpec =
  context "when we place images beside each other" $
    it "produces correct image" $
      checkWithFiles (\x -> beside [x,x])
        "data-examples/lenna.png"
        "data-examples/lenna-beside.png"

belowSpec :: Spec
belowSpec =
  context "when we place images below each other" $
    it "produces correct image" $
      checkWithFiles (\x -> below [x,x])
        "data-examples/lenna.png"
        "data-examples/lenna-below.png"

-- | Run given transforming function on image loaded from one file and
-- compare resulting image with contents of another file.

checkWithFiles
  :: (Image PixelRGB8 -> Image PixelRGB8) -- ^ Transformation to test
  -> FilePath          -- ^ Where to get the original image
  -> FilePath          -- ^ Where to get image to compare with
  -> Expectation
checkWithFiles f opath fpath = do
  (Right (ImageRGB8 original)) <- readImage opath
  (Right (ImageRGB8 result))   <- readImage fpath
  f original `blindlySatisfy` sameImage result

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
