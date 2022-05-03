import Data.Bits
import Data.Word

encodeDC :: Double -> Double -> Double -> Int
encodeDC r g b = fromIntegral . toInteger $ (r' `shift` 16) + (g' `shift` 8) + b'
  where
    r' = round r :: Word8
    g' = round g :: Word8
    b' = round b :: Word8

main = do
  let a = 4.0 :: Double
  let b = 5.0 :: Double
  let c = 6.0 :: Double

  let shifted = encodeDC a b c
  print shifted
