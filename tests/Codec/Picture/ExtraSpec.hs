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
  describe "crop"             cropSpec
  describe "flipHorizontally" flipHorizontallySpec
  describe "flipVertically"   flipVerticallySpec

cropSpec :: Spec
cropSpec =
  context "when we pass arguments within image size" $
    it "produces correct image" $ do
      (Right (ImageRGB8 original)) <- readImage "data-examples/lenna.png"
      (Right (ImageRGB8 cropped)) <- readImage "data-examples/lenna-cropped.png"
      crop 211 210 178 191 original `blindlySatisfy` sameImage cropped

flipHorizontallySpec :: Spec
flipHorizontallySpec =
  context "when we flip horizontally" $
    it "produces correct image" $ do
      (Right (ImageRGB8 original)) <- readImage "data-examples/lenna.png"
      (Right (ImageRGB8 flipped)) <-
        readImage "data-examples/lenna-horizontal-flip.png"
      flipHorizontally original `blindlySatisfy` sameImage flipped

flipVerticallySpec :: Spec
flipVerticallySpec =
  context "when we flip vertically" $
    it "produces correct image" $ do
      (Right (ImageRGB8 original)) <- readImage "data-examples/lenna.png"
      (Right (ImageRGB8 flipped)) <-
        readImage "data-examples/lenna-vertical-flip.png"
      flipVertically original `blindlySatisfy` sameImage flipped

blindlySatisfy :: a -> (a -> Bool) -> Expectation
v `blindlySatisfy` p =
  unless (p v) (expectationFailure "predicate failed")

sameImage :: Image PixelRGB8 -> Image PixelRGB8 -> Bool
sameImage a b =
  ((==) `on` imageWidth)  a b &&
  ((==) `on` imageHeight) a b &&
  ((==) `on` imageData)   a b
