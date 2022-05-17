import Blurhash.Encode
import Blurhash.Decode
import Codec.Picture
import System.Environment
import Options.Applicative

data Command = Encode String Int Int
             | Decode String Int Int String

encode :: Parser Command
encode = Encode
     <$> strArgument
        (  metavar "BLURHASH"
        <> help "Blurhash to be decoded into a PNG")
     <*> xComponents
     <*> yComponents

decode :: Parser Command
decode = Decode
     <$> strArgument
        (  metavar "IMAGE_PATH"
        <> help "Path to image to be encoded into a blurhash")
     <*> width
     <*> height
     <*> output

xComponents :: Parser Int
xComponents = option auto
             ( long "nx"
            <> short 'x'
            <> metavar "NX"
            <> value 3
            <> help "Number of x components (1-9) (default: 3)")

yComponents :: Parser Int
yComponents = option auto
             ( long "ny"
            <> short 'y'
            <> metavar "NY"
            <> value 3
            <> help "Number of y components (1-9) (default: 3)")

width :: Parser Int
width = argument auto
             ( metavar "W"
            <> help "Width in pixels")

height :: Parser Int
height = argument auto
             ( metavar "H"
            <> help "Height in pixels")

output :: Parser String
output = strOption
             ( long "out"
            <> short 'o'
            <> metavar "FILE"
            <> help "Output file for the generated PNG")

encOptions :: Mod CommandFields Command
encOptions =  command "encode" (info encode (progDesc "Encode an image into a hash"))

decOptions :: Mod CommandFields Command
decOptions =  command "decode" (info decode (progDesc "Decode a hash into an image"))

optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "blurhash command [arguments...] [flags...]"
              <> header "blurhash: a small program to encode images into blurhashes and decode hashes into blurred images")

versionOption = infoOption "0.1" (long "version" <> short 'v' <> help "Show Version")

programOptions :: Parser Command
programOptions = hsubparser (encOptions <> decOptions)

doEncode imgPath x y = do
  Right dynImg <- readImage imgPath -- Either String DynamicImage
  let img = convertRGB8 dynImg
  let (w, h) = (fromIntegral $ imageWidth img, fromIntegral $ imageHeight img)

  putStrLn . blurEncode (x, y) $ img

doDecode hash w h o = do
  let img = blurDecode hash (w, h)
  savePngImage o $ ImageRGB8 img
  putStrLn $ "Saved image to " ++ o

main = do
  opts <- execParser optsParser
  case opts of
    Encode file x y   -> doEncode file x y
    Decode hash w h o -> doDecode hash w h o

