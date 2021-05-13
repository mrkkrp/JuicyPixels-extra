## Juicy Pixels Extra 0.5.0

* Changed how edge pixels are calculated when scaling via bilinear
  interpolation is used. [See PR
  21](https://github.com/mrkkrp/JuicyPixels-extra/pull/21).

* Dropped support for GHC 8.2 and older.

## Juicy Pixels Extra 0.4.1

* Fixed left rotation so that `rotateLeft90 . rotateRight90` is now identity
  function.

* Dropped support for GHC 7.10.

## Juicy Pixels Extra 0.4.0

* Fix `scaleBilinear` so it works correctly with images that have 16-bit
  components.

## Juicy Pixels Extra 0.3.0

* Made the function `scaleBilinear` polymorphic in pixel type.

## Juicy Pixels Extra 0.2.2

* Fixed a bug in the `crop` function related to incorrect calculation of new
  height in some cases.

## Juicy Pixels Extra 0.2.1

* Documentation improvements.

## Juicy Pixels Extra 0.2.0

* Added `rotate180`, `beside`, and `below`.

## Juicy Pixels Extra 0.1.1

* Added benchmarks.

* Considerably improved performance of all functions (they are about ×15
  faster now).

## Juicy Pixels Extra 0.1.0

* Initial release.
