-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

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
  (hash83 1 nc) ++ (hash83 1 $ round qntAC) ++ (hash83 4 dc) ++ (foldr (\ac str -> (hash83 2 ac) ++ str) "" comps)
  where
    nc    = (nx - 1) + (ny - 1) * 9
    num_c = nx * ny

    -- should this `take` be before the dct2?
    dctR  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelR $ img
    dctG  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelG $ img
    dctB  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelB $ img

    -- average color
    dc    = encodeDC (dctR !! 0) (dctG !! 0) (dctB !! 0)

    maxAC = maximum [maximum dctR, maximum dctG, maximum dctB]

    -- this is from their code, not from their description
    qntAC = limit 0 82 $ maxAC * 166 - 0.5

    comps = drop 1 $ zipWith3 (encodeAC maxAC) dctR dctG dctB

encodeDC :: Double -> Double -> Double -> Int
encodeDC r g b = fromIntegral . toInteger $ (r' `shift` 16) + (g' `shift` 8) + b'
  where
    r' = round r :: Int
    g' = round g :: Int
    b' = round b :: Int

-- this is from their code, not their description
encodeAC :: Double -> Double -> Double -> Double -> Int
encodeAC maxV r g b = round $ r' * (19 * 19) + g' * 19 + b'
  where
    r' = limit 0 18 $ ((r / maxV) ** 0.5) * 9 + 9.5
    g' = limit 0 18 $ ((g / maxV) ** 0.5) * 9 + 9.5
    b' = limit 0 18 $ ((b / maxV) ** 0.5) * 9 + 9.5

srgbToLinear :: Double -> Double
srgbToLinear value = ((x' + 0.055) / 1.055) ** 2.4
  where
    x = value / 255.0
    x' = if x <= 0.04045
         then x / 12.92
         else x

linearToSrgb :: Double -> Double
linearToSrgb value = fromIntegral $ floor ret
  where
    x   = limit 0 1 value
    ret = if x == 0.0031308
          then x * 12.92 * 255 + 0.5
          else (1.055 * (x ** (1.0 / 2.4)) - 0.055) * 255 + 0.5

hash83 :: Int -> Int -> String
hash83 places value = hash83' 1 places value

hash83' :: Int -> Int -> Int -> String
hash83' cur places value 
  | cur > places = ""
  | otherwise     = res : (hash83' (cur + 1) places value)
    where
      cipher = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
      digit  = (value `div` (83 ^ (places - cur))) `mod` 83
      res    = cipher !! digit

main = do
  (imgPath:_) <- getArgs
  dynImg <- readImage imgPath -- Either String DynamicImage
  let img = toRGB8 dynImg

  let  (nx, ny) = (3, 3)
  let  num_c = nx * ny

  let  -- should this `take` be before the dct2?
  let  dctR  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelR $ img
  let  dctG  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelG $ img
  let  dctB  = take num_c . elems . amap linearToSrgb . dct2 . amap srgbToLinear . channelB $ img
  
  print . show $ dctR
  print . show $ dctG
  print . show $ dctB

  print $ encodeDC (head dctR) (head dctG) (head dctB)

  -- cat.jpg should get us "KXL41nIA~q_2x]S5xvW=M|" for (3, 3)
  print "KXL41nIA~q_2x]S5xvW=M|"
  print . blurhash (3, 3) $ img

