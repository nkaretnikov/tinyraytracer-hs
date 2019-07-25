-- https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing

{-# language OverloadedStrings #-}
{-# language NegativeLiterals #-}

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

data Sphere = Sphere
  { _center :: Vec3f
  , _radius :: {-# UNPACK #-} !Float
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

(*.) :: Vec3f -> Vec3f -> Float
(Vec3f x1 y1 z1) *. (Vec3f x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

(*..) :: Vec3f -> Float -> Vec3f
(Vec3f x y z) *.. f = Vec3f (x * f) (y * f) (z * f)

(-.) :: Vec3f -> Vec3f -> Vec3f
(Vec3f x1 y1 z1) -. (Vec3f x2 y2 z2) = Vec3f (x1 - x2) (y1 - y2) (z1 - z2)

-- http://web.archive.org/web/20170707080450/http://www.lighthouse3d.com/tutorials/maths/ray-sphere-intersection/
rayIntersect :: Sphere -> Vec3f -> Vec3f -> Bool
rayIntersect (Sphere center radius) orig dir
  | d2 > radius * radius = False
  | t1 < 0 && t2 < 0     = False
  | t1 < 0               = True
  | otherwise            = True
  where
    l :: Vec3f
    l = center -. orig

    tca :: Float
    tca = l *. dir

    d2 :: Float
    d2 = l *. l - tca * tca

    thc :: Float
    thc = sqrt $ radius * radius - d2

    t1 = tca - thc
    t2 = tca + thc

castRay :: Sphere -> Vec3f -> Vec3f -> Vec3f
castRay sphere orig dir =
  if rayIntersect sphere orig dir
  then Vec3f 0.4 0.4 0.3
  else Vec3f 0.2 0.7 0.8  -- background color

normalize :: Vec3f -> Vec3f
normalize v = v *.. (l  / norm v)
  where
    l = 1

    norm :: Vec3f -> Float
    norm (Vec3f x y z) = sqrt $ x * x + y * y + z * z

-- | Write an image to disk.
render :: FilePath -> Int -> Int -> Sphere -> IO ()
render file width height sphere =
  BSC.writeFile file (header <> image)
  where
    -- Field of view.
    fov :: Float
    fov = pi / 2

    fWidth :: Float
    fWidth = intToFloat width

    fHeight :: Float
    fHeight = intToFloat height

    framebuffer :: Vector Vec3f
    framebuffer =
      V.fromList $
        flip fmap (indexes height width) $ \(j,i) ->
          let fi   = intToFloat i
              fj   = intToFloat j
              x    =  (2 * (fi + 0.5) / fWidth  - 1) * tan (fov / 2) * fWidth / fHeight
              y    = -(2 * (fj + 0.5) / fHeight - 1) * tan (fov / 2)
              orig = Vec3f 0 0 0
              dir  = normalize $ Vec3f x y -1
          in castRay sphere orig dir

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
        (Just width, Just height) -> render file width height sphere
        _ -> usage
    _ -> usage
  where
    readInt :: String -> Maybe Int
    readInt = readMaybe

    sphere :: Sphere
    sphere = Sphere center radius

    center :: Vec3f
    center = Vec3f -3 0 -16

    radius :: Float
    radius = 2
