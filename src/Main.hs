-- https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing

{-# language OverloadedStrings #-}
{-# language NegativeLiterals #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Numeric.Limits (maxValue)
import System.Environment (getArgs, getProgName)
import Text.Read (readMaybe)

data Vec2f = Vec2f
  { _v2f_x :: {-# UNPACK #-} !Float
  , _v2f_y :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

data Vec3f = Vec3f
  { _v3f_x :: {-# UNPACK #-} !Float
  , _v3f_y :: {-# UNPACK #-} !Float
  , _v3f_z :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

data Material = Material
  { _albedo           :: Vec2f
  , _diffuseColor     :: Vec3f
  , _specularExponent :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

makeMaterial :: Material
makeMaterial = Material (Vec2f 1 0) (Vec3f 0 0 0) 0

data Sphere = Sphere
  { _center   :: Vec3f
  , _radius   :: {-# UNPACK #-} !Float
  , _material :: Material
  } deriving (Eq, Show)

data Light = Light
  { _position :: Vec3f
  , _intensity :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

-- XXX: May fail at runtime.
(!!.) :: Vec3f -> Int -> Float
(!!.) v 0 = _v3f_x v
(!!.) v 1 = _v3f_y v
(!!.) v 2 = _v3f_z v
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

(+.) :: Vec3f -> Vec3f -> Vec3f
(Vec3f x1 y1 z1) +. (Vec3f x2 y2 z2) = Vec3f (x1 + x2) (y1 + y2) (z1 + z2)

(-.) :: Vec3f -> Vec3f -> Vec3f
(Vec3f x1 y1 z1) -. (Vec3f x2 y2 z2) = Vec3f (x1 - x2) (y1 - y2) (z1 - z2)

minus :: Vec3f -> Vec3f
minus v = v *.. -1

-- http://web.archive.org/web/20170707080450/http://www.lighthouse3d.com/tutorials/maths/ray-sphere-intersection/
rayIntersect :: Sphere -> Vec3f -> Vec3f -> Float -> (Bool, Float)
rayIntersect (Sphere center radius _) orig dir t0
  | d2 > radius * radius = (False, t0)
  | t1 < 0 && t2 < 0     = (False, t2)
  | t1 < 0               = (True, t1)
  | otherwise            = (True, t1)
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

reflect :: Vec3f -> Vec3f -> Vec3f
reflect i n = i -. ((n *.. 2) *.. (i *. n))

sceneIntersect
  :: [Sphere] -> Material -> Vec3f -> Vec3f -> Vec3f -> Vec3f
  -> (Bool, Material, Vec3f, Vec3f)
sceneIntersect ss m o d p n = (sd < 1000, m', p', n')
  where
    (_, sd, m', p', n') = go 0 maxValue ss m o d p n

    go :: Float -> Float -> [Sphere] -> Material -> Vec3f -> Vec3f -> Vec3f -> Vec3f
       -> (Float, Float, Material, Vec3f, Vec3f)
    go distI spheresDist []               material _    _   point normal =
      (distI, spheresDist, material, point, normal)
    go distI spheresDist (sphere:spheres) material orig dir point normal =
      let (intersects, distI') = rayIntersect sphere orig dir distI
          material'            = _material sphere
          point'               = orig +. (dir *.. distI')
          normal'              = normalize $ (point' -. _center sphere)
      in if intersects && distI' < spheresDist
         then go distI' distI'      spheres material' orig dir point' normal'
         else go distI' spheresDist spheres material  orig dir point  normal

castRay :: [Sphere] -> [Light] -> Vec3f -> Vec3f -> Vec3f
castRay spheres lights orig dir =
  if intersects
  then ((_diffuseColor material' *.. diffuseLightIntensity') *..  _v2f_x (_albedo material')) +.
       ((Vec3f 1 1 1 *.. specularLightIntensity') *.. _v2f_y (_albedo material'))
  else Vec3f 0.2 0.7 0.8  -- background color
    where
      (intersects, material', point', n')
        = sceneIntersect spheres material orig dir point n
      (diffuseLightIntensity', specularLightIntensity')
        = go diffuseLightIntensity specularLightIntensity lights

      material :: Material
      material = makeMaterial

      n :: Vec3f
      n = Vec3f 0 0 0

      point :: Vec3f
      point = Vec3f 0 0 0

      diffuseLightIntensity :: Float
      diffuseLightIntensity = 0

      specularLightIntensity :: Float
      specularLightIntensity = 0

      go :: Float -> Float -> [Light] -> (Float, Float)
      go dli sli []     = (dli, sli)
      go dli sli (l:ls) =
        let lightDir = normalize $ (_position l) -. point'
            dli'     = dli + (_intensity l) * (max 0 (lightDir *. n'))
            refl     = minus $ reflect (minus lightDir) n'
            sli'     = sli + ((max 0 (refl *. dir)) ** (_specularExponent material'))
                     * _intensity l
        in go dli' sli' ls

normalize :: Vec3f -> Vec3f
normalize v = v *.. (l  / norm v)
  where
    l = 1

    norm :: Vec3f -> Float
    norm (Vec3f x y z) = sqrt $ x * x + y * y + z * z

-- | Write an image to disk.
render :: FilePath -> Int -> Int -> [Sphere] -> [Light] -> IO ()
render file width height spheres lights =
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
          in castRay spheres lights orig dir

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
          let c@(Vec3f x y z) = framebuffer V.! i
              fmax            = max x $ max y z
              c'              = c *.. (1 / fmax)
          in if (fmax > 1)
             then floatToWord8 $ (intToFloat ppmMaxVal) * (max 0 (min 1 (c' !!. j)))
             else floatToWord8 $ (intToFloat ppmMaxVal) * (max 0 (min 1 (c  !!. j)))

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
        (Just width, Just height) -> render file width height spheres lights
        _ -> usage
    _ -> usage
  where
    readInt :: String -> Maybe Int
    readInt = readMaybe

    ivory :: Material
    ivory = Material (Vec2f 0.6 0.3) (Vec3f 0.4 0.4 0.3) 50

    redRubber :: Material
    redRubber = Material (Vec2f 0.9 0.1) (Vec3f 0.3 0.1 0.1) 10

    spheres :: [Sphere]
    spheres =
      [ Sphere (Vec3f -3    0   -16) 2 ivory
      , Sphere (Vec3f -1.0 -1.5 -12) 2 redRubber
      , Sphere (Vec3f  1.5 -0.5 -18) 3 redRubber
      , Sphere (Vec3f  7    5   -18) 4 ivory
      ]

    lights :: [Light]
    lights =
      [ Light (Vec3f -20 20  20) 1.5
      , Light (Vec3f  30 50 -25) 1.8
      , Light (Vec3f  30 20  30) 1.7
      ]
