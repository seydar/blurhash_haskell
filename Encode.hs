-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

--{-# LANGUAGE FlexibleContexts    #-}

import Math.FFT
import Codec.Picture
import System.Environment
import Data.Array.CArray
import Control.Applicative
import Data.Bits

type Array2D = CArray (Int, Int) Double

toRGB8 :: Either String DynamicImage -> Image PixelRGB8
toRGB8 (Left _)  = undefined
toRGB8 (Right img) = convertRGB8 img

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | i <- [0..w], j <- [0..h]]
  where
    h = (imageHeight img) - 1
    w = (imageWidth img) - 1

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
        f' pixels = truncate (limit 0 255 $ pixels ! i)

--limit :: Double -> Double
limit low high x | x < low    = low
                 | x > high   = high
                 | otherwise  = x

-- dctImg img = fromChannels r' g' b'
--   where
--     r' = dct2 . channelR $ img
--     g' = dct2 . channelG $ img
--     b' = dct2 . channelB $ img

blurhash :: (Int, Int) -> Image PixelRGB8 -> String
blurhash (nx, ny) img =
  (hash83 1 nc) ++ (hash83 1 $ round qntAC) ++ (hash83 4 dc) ++ (map (hash83 2) compsAC)
  where
    nc   = (nx - 1) + (ny - 1) * 9

    -- should this `take` be before the dct2?
    dctR = take nc . elems . dct2 . channelR $ img
    dctG = take nc . elems . dct2 . channelG $ img
    dctB = take nc . elems . dct2 . channelB $ img

    -- average color
    dc   = encodeDC (dctR ! (0, 0)) (dctG ! (0, 0)) (dctB ! (0, 0))

    maxAC = maximum [maximum $ elems dctR, maximum $ elems dctG, maximum $ elems dctB]

    -- this is from their code, not from their description
    qntAC = limit 0 82 $ actual_max * 166 - 0.5

    comps = zipWith3 (encodeAC maxAC) dctR dctG dctB

-- FIXME this isn't correct because the underlying r g b values aren't correct.
-- they're supposed to be 0-255 (8-bits), but in fact are doubles.
-- this is my fault.
encodeDC :: Double -> Double -> Double -> Int
encodeDC r g b = fromIntegral . toInteger $ (r' `shift` 16) + (g' `shift` 8) + b'
  where
    r' = round r :: Pixel8
    g' = round g :: Pixel8
    b' = round b :: Pixel8

-- this is from their code, not their description
encodeAC :: Int -> Double -> Double -> Double -> Int
encodeAC maxV r g b = r' * (19 * 19) + g' * 19 + b'
  where
    r' = limit 0 18 $ ((r / maxV) `pow` 0.5) * 9 + 9.5
    g' = limit 0 18 $ ((g / maxV) `pow` 0.5) * 9 + 9.5
    b' = limit 0 18 $ ((b / maxV) `pow` 0.5) * 9 + 9.5

zipWith3C f xs ys zs =
  listArray (bounds xs) $ fmap (liftA3 f (xs !) (ys !) (zs !)) (range (bounds xs))

hash83 :: Int -> String
hash83 value = (cipher !! (fromIntegral value)) : []
  where
    cipher = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"

main = do
  (imgPath:_) <- getArgs
  dynImg <- readImage imgPath -- Either String DynamicImage
  let img = toRGB8 dynImg
  let (nx, ny) = (3, 3)
  let numComponents = (nx - 1) + (ny - 1) * 9
  let dctR = dct2 . channelR $ img
  let dctG = dct2 . channelG $ img
  let dctB = dct2 . channelB $ img
  let maxAC = maximum [maximum $ elems dctR, maximum $ elems dctG, maximum $ elems dctB]
  let avgColor = encodeDC (dctR ! (0, 0)) (dctG ! (0, 0)) (dctB ! (0, 0)) -- DC value
  let componentsAC =
        zipWith3C (\r g b -> r * (19 * 19) + g * 19 + b) dctR dctG dctB :: Array2D

  print . show $ numComponents
  print . show $ maxAC
  print . show $ dctR ! (0, 0)
  print . show $ avgColor -- DC value
  print . blurhash (3, 3) $ img

