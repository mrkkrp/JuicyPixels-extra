--
-- Benchmarks for the ‘JuicyPixels-extra’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
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

module Main (main) where

import Codec.Picture
import Codec.Picture.Extra
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "scaleBilinear"
    [ baction "512 × 512"   (scaleBilinear 512 512)   "data-examples/lenna.png"
    , baction "256 × 256"   (scaleBilinear 256 256)   "data-examples/lenna.png"
    , baction "1024 × 1024" (scaleBilinear 1024 1024) "data-examples/lenna.png" ]
  , bgroup "crop"
    [ baction "on 256 horizontally" (crop 256 0 256 512) "data-examples/lenna.png"
    , baction "on 265 vertically"   (crop 0 256 512 256) "data-examples/lenna.png" ]
  , bgroup "flipHorizontally"
    [ baction "512 × 512" flipHorizontally "data-examples/lenna.png"
    , baction "100 × 100" flipHorizontally "data-examples/lenna-scaled-down.png"
    ]
  , bgroup "flipVertically"
    [ baction "512 × 512" flipVertically "data-examples/lenna.png"
    , baction "100 × 100" flipVertically "data-examples/lenna-scaled-down.png" ]
  , bgroup "rotateLeft90"
    [ baction "512 × 512" rotateLeft90 "data-examples/lenna.png"
    , baction "100 × 100" rotateLeft90 "data-examples/lenna-scaled-down.png" ]
  , bgroup "rotateRight90"
    [ baction "512 × 512" rotateRight90 "data-examples/lenna.png"
    , baction "100 × 100" rotateRight90 "data-examples/lenna-scaled-down.png" ]
  ]

-- | Run a benchmark given function to benchmark and where to get image to
-- pass as an input.

baction
  :: String            -- ^ Name of the benchmark
  -> (Image PixelRGB8 -> Image PixelRGB8) -- ^ Transformation to perform
  -> FilePath          -- ^ Where to get image to start with
  -> Benchmark
baction name f path = env getImg (bench name . nf f)
  where
    getImg = do
      (Right (ImageRGB8 img)) <- readImage path
      return img
