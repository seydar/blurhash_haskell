import System.Environment

decode :: String -> (Int, Int) -> Image PixelRGB8
decode hash (w, h) = generateImage makePixel w h
  where
    basis x y = (cos (pi * i' * x / w)) * (cos (pi * j' * y / h))
    bases = [basis i j x y | j <- [0..ny - 1], i <- [0..nx - 1]]

main = do
  (hash:w:h:file:[]) <- getArgs
  let (width, height) = (read w, read h) :: (Int, Int)

  let img = decode hash (width, height)
  saveJpg img file

