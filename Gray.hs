import Codec.Picture
import System.Environment
import Math.FFT
import Data.Array.CArray

type Array2D = CArray (Int, Int) Double

grayscale :: PixelRGB8 -> Pixel8
grayscale (PixelRGB8 r g b) =
  linearToSrgb $ (0.2126 * r' + 0.7152 * g' + 0.0722 * b') / 255.0
  where
    r' = fromIntegral r
    g' = fromIntegral g
    b' = fromIntegral b

srgbToLinear :: Double -> Double
srgbToLinear value = ((x' + 0.055) / 1.055) ** 2.4
  where
    x = value / 255.0
    x' = if x <= 0.04045
         then x / 12.92
         else x

linearToSrgb :: Double -> Pixel8
linearToSrgb value = floor ret
  where
    x   = limit 0 1 value
    ret = if x <= 0.0031308
          then x * 12.92 * 255 + 0.5
          else (1.055 * (x ** (1.0 / 2.4)) - 0.055) * 255 + 0.5

limit low high x | x < low    = low
                 | x > high   = high
                 | otherwise  = x

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | i <- [0..w], j <- [0..h]]
  where
    h = (imageHeight img) - 1
    w = (imageWidth img) - 1

toCArray :: Image Pixel8 -> Array2D
toCArray img@(Image w h _) =
  listArray ((0, 0), (w - 1, h - 1)) .
  map (fromIntegral . toInteger) .
  oneDim $ img

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

main = do
  (imgPath:_)  <- getArgs
  Right dynImg <- readImage imgPath
  let img = convertRGB8 dynImg

  let gray = pixelMap grayscale img
  let red = pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 r 0 0) img
  let green = pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 g 0) img
  let blue = pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 0 0 b) img

  let fft = dht $ toCArray gray :: Array2D
  let (w, h) = (imageWidth img, imageHeight img)
  let fft_img = generateImage (\x y -> linearToSrgb $ fft ! (x, y)) w h

  saveJpgImage 100 "red_cat.jpg" $ ImageRGB8 red
  saveJpgImage 100 "green_cat.jpg" $ ImageRGB8 green
  saveJpgImage 100 "blue_cat.jpg" $ ImageRGB8 blue
  saveJpgImage 100 "dct_cat.jpg" $ ImageY8 fft_img

