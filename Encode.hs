-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

import Math.FFT
import Codec.Picture
import System.Environment
import Data.Array.CArray
import Control.Applicative
import Data.Bits

type Array2D = CArray (Int, Int) Double

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
    r' = limit 0 18 $ (sqrt (r / maxV)) * 9 + 9.5
    g' = limit 0 18 $ (sqrt (g / maxV)) * 9 + 9.5
    b' = limit 0 18 $ (sqrt (b / maxV)) * 9 + 9.5

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
    ret = if x <= 0.0031308
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

grayscale :: PixelRGB8 -> Double
grayscale (PixelRGB8 r g b) = (0.2126 * r' + 0.7152 * g' + 0.0722 * b') / 255.0
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b

main = do
  (imgPath:_) <- getArgs
  Right dynImg <- readImage imgPath -- Either String DynamicImage
  let img = convertRGB8 dynImg

  let  (nx, ny) = (3, 3)
  let  num_c = nx * ny
  let (w, h) = (fromIntegral $ imageWidth img, fromIntegral $ imageHeight img) :: (Double, Double)

  -- should this `take` be before the dct2?
  -- FIXME maybe multiply the dct2 output by the original input?
  let  dctR  = take num_c . elems . dct2 . channelR $ img
  let  dctG  = take num_c . elems . dct2 . channelG $ img
  let  dctB  = take num_c . elems . dct2 . channelB $ img

  let doc = take num_c . elems . dct2 . channelR $ img
  
  print (w, h)
  print . show $ (channelR img) ! (4, 4)
  print . show $ doc

  print $ encodeDC (head dctR) (head dctG) (head dctB)

  -- cat.jpg should get us "KXL41nIA~q_2x]S5xvW=M|" for (3, 3)
  print "KXL41nIA~q_2x]S5xvW=M|"
  print . blurhash (3, 3) $ img

