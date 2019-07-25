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

-- | Return a list of indexes of a 2d array.
indexes :: Int -> Int -> [(Int,Int)]
indexes height width = go 0 0 (width * height) height width
  where
    go i j n h w
      | n <= 0    = []
      | i >= w    =         go 0       (j + 1) n       h w
      | j >= h    =         go (i + 1) 0       n       h w
      | otherwise = (j,i) : go (i + 1) j       (n - 1) h w

-- XXX: 'String' concatenation is inefficient.
-- | Write an image to disk.
render :: FilePath -> Int -> Int -> IO ()
render file width height = writeFile file (header ++ image ++ footer)
  where
    framebuffer :: [Vec3f]
    framebuffer =
      flip fmap (indexes height width) $ \(j,i) ->
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
        -- XXX: Take advantage of the Haskell 'Vec3f' representation.
        -- In the original C++ code, 'Vec3f' is just an array of size 3.
        flip fmap (indexes (width * height) 3) $ \(i,j) ->
          show $ floatToInt $ 255 * (max 0 (min 1 (framebuffer !! i !!. j)))

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
