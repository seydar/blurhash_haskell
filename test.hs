import Blurhash.Encode
import Codec.Picture
import System.Environment

main = do
  (nx:ny:imgPath:_) <- getArgs
  Right dynImg <- readImage imgPath -- Either String DynamicImage
  let img = convertRGB8 dynImg
  let (w, h) = (fromIntegral $ imageWidth img, fromIntegral $ imageHeight img)
  let (x, y) = (read nx, read ny) :: (Int, Int)

  -- cat.jpg should get us "KYLDf7IA~p^+x]S5xvW=M|" for (3, 3)
  -- cat.jpg should get us "ARLDf7Di~W%M" for (2, 2)
  putStrLn $ "Test program!"
  putStrLn $ "Generating blurhash for " ++ show w ++ "x" ++ show h ++ " image..."
  putStrLn . blurhash (x, y) $ img

