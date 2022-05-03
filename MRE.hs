-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

import Codec.Picture
import System.Environment
import Data.Array.CArray
import Control.Applicative

type Array2D = CArray (Int, Int) Double

toRGB8 :: Either String DynamicImage -> Image PixelRGB8
toRGB8 (Left _)  = undefined
toRGB8 (Right img) = convertRGB8 img

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | i <- [0..w], j <- [0..h]]
  where
    h = (imageHeight img) - 1
    w = (imageWidth img) - 1

-- FIXME `listArray` line creates an array that has the index out of bounds
extractChannel :: (PixelRGB8 -> Pixel8) -> Image PixelRGB8 -> Array2D
extractChannel f img@(Image w h _) =
  listArray ((0, 0), (h - 1, w - 1)) . -- THIS IS WHERE THE ERROR HAPPENS
  map (fromIntegral . toInteger . f) .
  oneDim $ img

extractChannel' :: (PixelRGB8 -> Pixel8) -> Image PixelRGB8 -> [Pixel8]
extractChannel' f img@(Image w h _) =
  map (fromIntegral . toInteger . f) .
  oneDim $ img

pixelR, pixelG, pixelB :: PixelRGB8 -> Pixel8
pixelR (PixelRGB8 r _ _) = r
pixelG (PixelRGB8 _ g _) = g
pixelB (PixelRGB8 _ _ b) = b

channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB

channelR' = extractChannel' pixelR

main = do
  (imgPath:_) <- getArgs
  dynImg <- readImage imgPath -- Either String DynamicImage
  let img = toRGB8 dynImg

  print . show . imageWidth $ img
  print . show . imageHeight $ img
  print . show . length $ channelR' img
  print . show . bounds $ channelR img

