-- https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing

import Data.List (intercalate)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

data Vec3f = Vec3f
  { _x :: Float
  , _y :: Float
  , _z :: Float
  } deriving (Eq, Show)

-- XXX: May fail at runtime.
(!!.) :: Vec3f -> Int -> Float
(!!.) v 0 = _x v
(!!.) v 1 = _y v
(!!.) v 2 = _z v
(!!.) _ _ = error "invalid index"

intToFloat :: Int -> Float
intToFloat = fromIntegral

floatToInt :: Float -> Int
floatToInt = fromEnum

-- | Write an image to disk.
render :: FilePath -> Int -> Int -> IO ()
render file width height = writeFile file (header ++ image ++ footer)
  where
    framebuffer :: [Vec3f]
    framebuffer =
      concat $
        flip fmap [0..height - 1] $ \j ->
        flip fmap [0..width - 1]  $ \i ->
          Vec3f (intToFloat j / intToFloat height)
                (intToFloat i / intToFloat width)
                0

    -- XXX: Use the ASCII version to avoid dealing with bytestrings.
    -- https://en.wikipedia.org/wiki/Netpbm_format#File_format_description
    header :: String
    header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"

    image :: String
    image =
      intercalate " " $
      concat $
        flip fmap [0..height * width - 1] $ \i ->
        -- XXX: Take advantage of the Haskell 'Vec3f' representation.
        -- In the original C++ code, 'Vec3f' is just an array of size 3.
        flip fmap [0..3 - 1] $ \j ->
          show(floatToInt(255 * max 0 (min 1 (framebuffer !! i !!. j))))

    footer :: String
    footer = "\n"  -- XXX: required for the ASCII version

usage :: IO ()
usage = do
  prog <- getProgName
  putStrLn $ "./" ++ prog ++ " PPM_FILE WIDTH HEIGHT"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file, mwidth, mheight] ->
      case (readInt mwidth, readInt mheight) of
        (Just width, Just height) -> render file width height
        _ -> usage
    _ -> usage
  where
    readInt :: String -> Maybe Int
    readInt = readMaybe
