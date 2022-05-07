import System.Environment
import Codec.Picture
import Data.List
import Data.Maybe
import Data.Bits

type RGB = (Double, Double, Double)

limit :: Ord a => a -> a -> a -> a
limit low high x | x < low    = low
                 | x > high   = high
                 | otherwise  = x

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

unhash :: String -> Int -> Int -> Int
unhash hash start end = unhash83 slice
  where
    slice = take (end - start) . drop start $ hash

unhash83 :: String -> Int
unhash83 hash = total
  where
    cipher = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"

    -- this is a list of Maybes
    elems = reverse . catMaybes $ map (\h -> elemIndex h cipher) hash
    total = fst $ foldl (\(s, n) v -> (s + v * 83 ^ n, n + 1)) (0, 0) elems

numComponents :: String -> (Int, Int)
numComponents hash = (nx, ny)
  where
    size = unhash hash 0 1
    ny = (size `div`  9) + 1
    nx = (size `mod` 9) + 1

maxValue :: String -> Double
maxValue hash = (fromIntegral qnt + 1) / 166
  where
    qnt = unhash hash 1 2

decodeDC :: String -> RGB
decodeDC hash = (srgbToLinear r, srgbToLinear g, srgbToLinear b)
  where
    avgColor = unhash hash 2 6
    r = fromIntegral $ avgColor `shift` (-16)
    g = fromIntegral $ (avgColor `shift` (-8)) .&. 255
    b = fromIntegral $ avgColor .&. 255

signpow v e = (signum v) * (abs v) ** e

decodeAC :: Double -> Int -> RGB
decodeAC maxV ac = (r', g', b')
  where
    r  = fromIntegral $ ac `div` (19 * 19)
    g  = fromIntegral $ (ac `div` 19) `mod` 19
    b  = fromIntegral $ ac `mod` 19
    r' = (signpow ((r - 9) / 9) 2.0) * maxV
    g' = (signpow ((g - 9) / 9) 2.0) * maxV
    b' = (signpow ((b - 9) / 9) 2.0) * maxV

decodeACs:: String -> [RGB]
decodeACs hash = (decodeDC hash) : acs
  where
    (nx, ny) = numComponents hash

    -- this would normally go from 0..(nx * ny) - 1
    -- but since we already have the first component from `decodeDC`, we
    -- only need 1..(nx * ny) - 1
    acValues = [unhash hash (4 + x * 2) (6 + x * 2) | x <- [1..(nx * ny) - 1]]
    acs = map (decodeAC (maxValue hash)) acValues

decode :: String -> (Int, Int) -> Image PixelRGB8
decode hash (w, h) = generateImage (makePixel acs (w, h) (nx, ny)) w h
  where
    (nx, ny) = numComponents hash
    acs = decodeACs hash

makePixel :: [RGB] -> (Int, Int) -> (Int, Int) -> Int -> Int -> PixelRGB8
makePixel acs (w, h) (nx, ny) x y = PixelRGB8 r' g' b'
  where
    basis i j = (cos (pi * i' * x' / w')) * (cos (pi * j' * y' / h'))
      where
        i' = fromIntegral i
        j' = fromIntegral j
        x' = fromIntegral x
        y' = fromIntegral y
        w' = fromIntegral w
        h' = fromIntegral h
    bases = [basis i j | j <- [0..ny - 1], i <- [0..nx - 1]]

    reds   = map (\(r, _, _) -> r) acs
    greens = map (\(_, g, _) -> g) acs
    blues  = map (\(_, _, b) -> b) acs

    r = sum $ zipWith (*) bases reds
    g = sum $ zipWith (*) bases greens
    b = sum $ zipWith (*) bases blues

    r' = fromIntegral . limit 0 255 $ linearToSrgb r
    g' = fromIntegral . limit 0 255 $ linearToSrgb g
    b' = fromIntegral . limit 0 255 $ linearToSrgb b

main = do
  (hash:w:h:file:[]) <- getArgs
  let (width, height) = (read w, read h) :: (Int, Int)

  let img = decode hash (width, height)
  savePngImage file $ ImageRGB8 img
  putStrLn $ "Saved image to " ++ file

