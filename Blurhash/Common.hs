module Blurhash.Common where

type RGB = (Double, Double, Double)

limit :: Ord a => a -> a -> a -> a
limit low high x | x < low    = low
                 | x > high   = high
                 | otherwise  = x

signpow v e = (signum v) * (abs v) ** e

srgbToLinear :: Double -> Double
srgbToLinear value = if x <= 0.04045
                     then x / 12.92
                     else ((x + 0.055) / 1.055) ** 2.4
  where
    x = value / 255.0

linearToSrgb :: Double -> Int
linearToSrgb value = floor ret
  where
    x   = limit 0 1 value
    ret = if x <= 0.0031308
          then x * 12.92 * 255 + 0.5
          else (1.055 * (x ** (1.0 / 2.4)) - 0.055) * 255 + 0.5


