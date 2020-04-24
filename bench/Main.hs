module Main (main) where

import Codec.Picture
import Codec.Picture.Extra
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "scaleBilinear"
        [ baction "512 × 512" (scaleBilinear 512 512) "data-examples/lenna.png",
          baction "256 × 256" (scaleBilinear 256 256) "data-examples/lenna.png",
          baction "1024 × 1024" (scaleBilinear 1024 1024) "data-examples/lenna.png"
        ],
      bgroup
        "crop"
        [ baction "on 256 horizontally" (crop 256 0 256 512) "data-examples/lenna.png",
          baction "on 265 vertically" (crop 0 256 512 256) "data-examples/lenna.png"
        ],
      bgroup
        "flipHorizontally"
        [ baction "512 × 512" flipHorizontally "data-examples/lenna.png",
          baction "100 × 100" flipHorizontally "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "flipVertically"
        [ baction "512 × 512" flipVertically "data-examples/lenna.png",
          baction "100 × 100" flipVertically "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "rotateLeft90"
        [ baction "512 × 512" rotateLeft90 "data-examples/lenna.png",
          baction "100 × 100" rotateLeft90 "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "rotateRight90"
        [ baction "512 × 512" rotateRight90 "data-examples/lenna.png",
          baction "100 × 100" rotateRight90 "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "rotate180"
        [ baction "512 × 512" rotate180 "data-examples/lenna.png",
          baction "100 × 100" rotate180 "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "beside"
        [ baction "1024 × 512" beside' "data-examples/lenna.png",
          baction "200 × 100" beside' "data-examples/lenna-scaled-down.png"
        ],
      bgroup
        "below"
        [ baction "512 × 1024" below' "data-examples/lenna.png",
          baction "100 × 200" below' "data-examples/lenna-scaled-down.png"
        ]
    ]
  where
    beside' img = beside [img, img]
    below' img = below [img, img]

-- | Run a benchmark given function to benchmark and where to get image to
-- pass as an input.
baction ::
  -- | Name of the benchmark
  String ->
  -- | Transformation to perform
  (Image PixelRGB8 -> Image PixelRGB8) ->
  -- | Where to get image to start with
  FilePath ->
  Benchmark
baction name f path = env getImg (bench name . nf f)
  where
    getImg = do
      (Right (ImageRGB8 img)) <- readImage path
      return img
