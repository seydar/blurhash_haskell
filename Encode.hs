module Encode (blurhash) where

import Math.FFT
import Codec.Picture
import System.Environment
import Control.Applicative
import Data.Bits

type RGB = (Double, Double, Double)

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | j <- [0..h], i <- [0..w]]
  where
    h = (imageHeight img) - 1
    w = (imageWidth img) - 1

pixelR, pixelG, pixelB :: PixelRGB8 -> Pixel8
pixelR (PixelRGB8 r _ _) = r
pixelG (PixelRGB8 _ g _) = g
pixelB (PixelRGB8 _ _ b) = b

extractChannel :: (PixelRGB8 -> Pixel8) -> Image PixelRGB8 -> [Pixel8]
extractChannel f img@(Image w h _) = map f . oneDim $ img

channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB

--limit :: Double -> Double
limit low high x | x < low    = low
                 | x > high   = high
                 | otherwise  = x

manualDCT :: Image PixelRGB8 -> (Int, Int) -> [RGB]
manualDCT img (nx, ny) = [basisFunction img i j | j <- [0..ny - 1], i <- [0..nx - 1]]

basisFunction :: Image PixelRGB8 -> Int -> Int -> RGB
basisFunction img i j = (r, g, b)
  where
    (w, h) = (fromIntegral $ imageWidth img, fromIntegral $ imageHeight img)
    --(w, h) = (10, 11)
    (i', j') = (fromIntegral i, fromIntegral j)

    -- not sure why `x` and `y` are Doubles, but I'm not mad about it
    basis x y = (cos (pi * i' * x / w)) * (cos (pi * j' * y / h))
    bases = [basis x y | y <- [0..h - 1], x <- [0..w - 1]] :: [Double]

    red   = zipWith (*) bases . map (srgbToLinear . fromIntegral) . channelR $ img
    green = zipWith (*) bases . map (srgbToLinear . fromIntegral) . channelG $ img
    blue  = zipWith (*) bases . map (srgbToLinear . fromIntegral) . channelB $ img

    norm   = if i == 0 && j == 0
             then 1.0
             else 2.0
    scale = norm / (w * h)
    r = scale * (sum red)
    g = scale * (sum green)
    b = scale * (sum blue)

blurhash :: (Int, Int) -> Image PixelRGB8 -> String
blurhash (nx, ny) img =
  (hash83 1 nc) ++ (hash83 1 qntAC) ++ (hash83 4 dc) ++ (foldr (\ac str -> (hash83 2 ac) ++ str) "" comps)
  where
    nc    = (nx - 1) + (ny - 1) * 9
    num_c = nx * ny

    dct   = take num_c . manualDCT img $ (nx, ny)
    acs   = drop 1 dct

    -- average color
    dc    = encodeDC $ head dct

    maxAC = foldl (\m (a, b, c) -> maximum $ map abs [m, a, b, c]) 0.0 acs

    qntAC  = limit 0 82 . floor $ maxAC * 166 - 0.5 :: Int
    maxAC' = ((fromIntegral qntAC) + 1) / 166 :: Double

    comps = map (encodeAC maxAC') acs

encodeDC :: RGB -> Int
encodeDC (r, g, b) = fromIntegral . toInteger $ (r' `shift` 16) + (g' `shift` 8) + b'
  where
    r' = round . linearToSrgb $ r :: Int
    g' = round . linearToSrgb $ g :: Int
    b' = round . linearToSrgb $ b :: Int

-- this is from their code, not their description
encodeAC :: Double -> RGB -> Int
encodeAC maxV (r, g, b) = r' * (19 * 19) + g' * 19 + b'
  where
    r' = limit 0 18 . floor $ (signpow (r / maxV) 0.5) * 9 + 9.5
    g' = limit 0 18 . floor $ (signpow (g / maxV) 0.5) * 9 + 9.5
    b' = limit 0 18 . floor $ (signpow (b / maxV) 0.5) * 9 + 9.5

signpow v e = (signum v) * (abs v) ** e

srgbToLinear :: Double -> Double
srgbToLinear value = if x <= 0.04045
                     then x / 12.92
                     else ((x + 0.055) / 1.055) ** 2.4
  where
    x = value / 255.0

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

main = do
  (nx:ny:imgPath:_) <- getArgs
  Right dynImg <- readImage imgPath -- Either String DynamicImage
  let img = convertRGB8 dynImg
  let (w, h) = (fromIntegral $ imageWidth img, fromIntegral $ imageHeight img)
  let (x, y) = (read nx, read ny) :: (Int, Int)

  -- cat.jpg should get us "KYLDf7IA~p^+x]S5xvW=M|" for (3, 3)
  -- cat.jpg should get us "ARLDf7Di~W%M" for (2, 2)
  putStrLn $ "Generating blurhash for " ++ show w ++ "x" ++ show h ++ " image..."
  putStrLn . blurhash (x, y) $ img

