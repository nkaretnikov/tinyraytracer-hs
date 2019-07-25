-- https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing

{-# language OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

data Vec3f = Vec3f
  { _x :: {-# UNPACK #-} !Float
  , _y :: {-# UNPACK #-} !Float
  , _z :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

-- XXX: May fail at runtime.
(!!.) :: Vec3f -> Int -> Float
(!!.) v 0 = _x v
(!!.) v 1 = _y v
(!!.) v 2 = _z v
(!!.) _ _ = error "invalid index"

intToFloat :: Int -> Float
intToFloat = fromIntegral

-- floatToInt :: Float -> Int
-- floatToInt = fromEnum

floatToWord8 :: Float -> Word8
floatToWord8 = fromIntegral . fromEnum

-- | Return a list of indexes of a 2d array.
indexes :: Int -> Int -> [(Int,Int)]
indexes height width = go 0 0 (width * height) height width
  where
    go i j n h w
      | n <= 0    = []
      | i >= w    =         go 0       (j + 1) n       h w
      | j >= h    =         go (i + 1) 0       n       h w
      | otherwise = (j,i) : go (i + 1) j       (n - 1) h w

-- | Write an image to disk.
render :: FilePath -> Int -> Int -> IO ()
render file width height = BSC.writeFile file (header <> image)
  where
    framebuffer :: Vector Vec3f
    framebuffer =
      V.fromList $
        flip fmap (indexes height width) $ \(j,i) ->
            Vec3f (intToFloat j / intToFloat height)
                  (intToFloat i / intToFloat width)
                  0

    ppmMaxVal :: Int
    ppmMaxVal = 255

    header :: ByteString
    header =
      BSC.concat
        [ "P6\n"
        , BSC.pack (show width)
        , " "
        , BSC.pack (show height)
        , "\n"
        , BSC.pack (show ppmMaxVal)
        , "\n"
        ]

    image :: ByteString
    image =
      BS.pack $
        -- XXX: Take advantage of the Haskell 'Vec3f' representation.
        -- In the original C++ code, 'Vec3f' is just an array of size 3.
        flip fmap (indexes (width * height) 3) $ \(i,j) ->
          floatToWord8 $ (intToFloat ppmMaxVal) * (max 0 (min 1 (framebuffer V.! i !!. j)))

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
