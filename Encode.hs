-- heavily inspired/branched from
-- https://github.com/phadej/JuicyPixels-scale-dct/blob/master/src/Codec/Picture/ScaleDCT.hs

import Math.FFT
import Codec.Picture
import System.Environment
import Data.Array.CArray
import Control.Applicative

type Array2D = CArray (Int, Int) Double

toRGBA8 :: Either String DynamicImage -> Image PixelRGBA8
toRGBA8 (Left _)  = undefined
toRGBA8 (Right img) = convertRGBA8 img

oneDim :: (Pixel p) => Image p -> [p]
oneDim img = [pixelAt img i j | i <- [0..imageWidth img], j <- [0..imageHeight img]]

pixelR, pixelG, pixelB, pixelA :: PixelRGBA8 -> Pixel8
pixelR (PixelRGBA8 r _ _ _) = r
pixelG (PixelRGBA8 _ g _ _) = g
pixelB (PixelRGBA8 _ _ b _) = b
pixelA (PixelRGBA8 _ _ _ a) = a

extractChannel :: (PixelRGBA8 -> Pixel8) -> Image PixelRGBA8 -> Array2D
extractChannel f img@(Image w h _) =
  listArray ((0, 0), (h - 1, w - 1)) .
  map (fromIntegral . toInteger . f) .
  oneDim $ img

channelR = extractChannel pixelR
channelG = extractChannel pixelG
channelB = extractChannel pixelB
channelA = extractChannel pixelA

fromChannels r g b a = generateImage f w h
  where
    (_, (h', w')) = bounds r
    w = w' + 1
    h = h' + 1
    f x y = PixelRGBA8 (f' r) (f' g) (f' b) (f' a)
      where
        i = (y, x)
        f' pixels = truncate (pixels ! i)

dctImg img = fromChannels r' g' b' a'
  where
    r' = dct2 . channelR $ img
    g' = dct2 . channelG $ img
    b' = dct2 . channelB $ img
    a' = dct2 . channelA $ img

blurhash (nx, ny) img = generateHash numComponents maxAC avgColor componentsAC
  where
    numComponents = (nx - 1) + (ny - 1) * 9
    dctR = dct2 . channelR $ img
    dctG = dct2 . channelG $ img
    dctB = dct2 . channelB $ img
    maxAC = maximum [maximum dctR, maximum dctG, maximum dctB]
    avgColor = combineRGB (dctR ! (0, 0)) (dctG ! (0, 0)) (dctB ! (0, 0)) -- DC value
    componentsAC =
      zipWith3C (\r g b -> r * (19 * 19) + g * 19 + b) dctR dctG dctB

combineRGB r g b = r + g + b

generateHash nc maxAC avgColor comps =
  (show nc) ++ (show maxAC) ++ (show avgColor) ++ (show . length $ comps)

zipWith3C f xs ys zs =
  listArray (bounds xs) $ fmap (liftA2 f (xs !) (ys !) (zs !)) (range (bounds xs))

main = do
  (imgPath:_) <- getArgs
  dynImg <- readImage imgPath -- Either String DynamicImage
  let img = toRGBA8 dynImg

  print . ("cool " ++) . show . length . oneDim $ img

