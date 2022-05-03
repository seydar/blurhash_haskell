-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

--{-# LANGUAGE FlexibleContexts    #-}

import Math.FFT
import Codec.Picture
import System.Environment
import Data.Array.CArray
import Control.Applicative

type Array2D = CArray (Int, Int) Double

toRGB8 :: Either String DynamicImage -> Image PixelRGB8
toRGB8 (Left _)  = undefined
toRGB8 (Right img) = convertRGB8 img

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | i <- [0..imageWidth img], j <- [0..imageHeight img]]

pixelR, pixelG, pixelB :: PixelRGB8 -> Pixel8
pixelR (PixelRGB8 r _ _) = r
pixelG (PixelRGB8 _ g _) = g
pixelB (PixelRGB8 _ _ b) = b

extractChannel :: (PixelRGB8 -> Pixel8) -> Image PixelRGB8 -> Array2D
extractChannel f img@(Image w h _) =
  listArray ((0, 0), (w - 1, h - 1)) .
  map (fromIntegral . toInteger . f) .
  oneDim $ img

channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB

fromChannels r g b a = generateImage f w h
  where
    (_, (h', w')) = bounds r
    w = w' + 1
    h = h' + 1
    f x y = PixelRGB8 (f' r) (f' g) (f' b)
      where
        i = (y, x)
        f' pixels = truncate (limit $ pixels ! i)

--limit :: Double -> Double
limit x | x < 0     = 0
        | x > 255   = 255
        | otherwise = x

-- dctImg img = fromChannels r' g' b' a'
--   where
--     r' = dct2 . channelR $ img
--     g' = dct2 . channelG $ img
--     b' = dct2 . channelB $ img
--     a' = dct2 . channelA $ img

-- blurhash :: (Int, Int) -> Image PixelRGBA8 -> String
-- blurhash (nx, ny) img = generateHash numComponents maxAC avgColor componentsAC
--   where
--     numComponents = (nx - 1) + (ny - 1) * 9
--     dctR = dct2 . channelR $ img
--     dctG = dct2 . channelG $ img
--     dctB = dct2 . channelB $ img
--     maxAC = maximum [maximum $ elems dctR, maximum $ elems dctG, maximum $ elems dctB]
--     avgColor = combineRGB (dctR ! (0, 0)) (dctG ! (0, 0)) (dctB ! (0, 0)) -- DC value
--     componentsAC =
--       zipWith3C (\r g b -> r * (19 * 19) + g * 19 + b) dctR dctG dctB :: Array2D
--       --zipWith3 (\r g b -> r * (19 * 19) + g * 19 + b) (elems dctR) (elems dctG) (elems dctB)

combineRGB r g b = r + g + b

-- TODO make this actually correct
generateHash nc maxAC avgColor comps =
  (show nc) ++ (show maxAC) ++ (show avgColor) ++ (show . bounds $ comps)
  --(show nc) ++ (show maxAC) ++ (show avgColor) ++ (show . length $ comps)

zipWith3C f xs ys zs =
  listArray (bounds xs) $ fmap (liftA3 f (xs !) (ys !) (zs !)) (range (bounds xs))

main = do
  (imgPath:_) <- getArgs
  dynImg <- readImage imgPath -- Either String DynamicImage
  let img = toRGB8 dynImg
  let (nx, ny) = (3, 3)
  let numComponents = (nx - 1) + (ny - 1) * 9
  --let dctR = dct2 . channelR $ img
  --let dctG = dct2 . channelG $ img
  --let dctB = dct2 . channelB $ img
  --let maxAC = maximum [maximum $ elems dctR, maximum $ elems dctG, maximum $ elems dctB]
  --let avgColor = combineRGB (dctR ! (0, 0)) (dctG ! (0, 0)) (dctB ! (0, 0)) -- DC value
  --let componentsAC =
  --      zipWith3C (\r g b -> r * (19 * 19) + g * 19 + b) dctR dctG dctB :: Array2D

  --print . ("cool " ++) . show . blurhash (3, 3) $ img
  print . show . bounds $ channelR img

