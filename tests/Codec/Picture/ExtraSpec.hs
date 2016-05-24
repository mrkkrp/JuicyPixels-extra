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
spec =
  describe "crop" cropSpec

cropSpec :: Spec
cropSpec =
  context "when we pass arguments within image size" $
    it "produces correct image" $ do
      (Right (ImageRGB8 original)) <- readImage "data-examples/lenna.png"
      (Right (ImageRGB8 cropped)) <- readImage "data-examples/lenna-cropped.png"
      let result = crop 211 210 178 191 original
      result `blindlySatisfy` sameImage cropped

blindlySatisfy :: a -> (a -> Bool) -> Expectation
v `blindlySatisfy` p =
  unless (p v) (expectationFailure "predicate failed")

sameImage :: Image PixelRGB8 -> Image PixelRGB8 -> Bool
sameImage a b =
  ((==) `on` imageWidth)  a b &&
  ((==) `on` imageHeight) a b &&
  ((==) `on` imageData)   a b
