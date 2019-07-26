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

-- data Vec2f = Vec2f
--   { _v2f_x :: {-# UNPACK #-} !Float
--   , _v2f_y :: {-# UNPACK #-} !Float
--   } deriving (Eq, Show)

data Vec3f = Vec3f
  { _v3f_x :: {-# UNPACK #-} !Float
  , _v3f_y :: {-# UNPACK #-} !Float
  , _v3f_z :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

data Vec4f = Vec4f
  { _v4f_x :: {-# UNPACK #-} !Float
  , _v4f_y :: {-# UNPACK #-} !Float
  , _v4f_z :: {-# UNPACK #-} !Float
  , _v4f_w :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

data Material = Material
  { _refractiveIndex  :: {-# UNPACK #-} !Float
  , _albedo           :: Vec4f
  , _diffuseColor     :: Vec3f
  , _specularExponent :: {-# UNPACK #-} !Float
  } deriving (Eq, Show)

makeMaterial :: Material
makeMaterial = Material 1 (Vec4f 1 0 0 0) (Vec3f 0 0 0) 0

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
  | t1 < 0               = (True, t2)
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

-- | Snell's law, see <https://en.wikipedia.org/wiki/Snell%27s_law>.
refract :: Vec3f -> Vec3f -> Float -> Vec3f
refract i n refractiveIndex =
  if k < 0
  then Vec3f 0 0 0
  else (i *.. eta) +. (n' *.. (eta * cosi' - sqrt k))
  where
    cosi :: Float
    cosi = negate $ max -1 (min 1 (i *. n))

    etai :: Float
    etai = 1

    etat :: Float
    etat = refractiveIndex

    -- If the ray is inside the object, swap the indexes and invert the normal
    -- to get the correct result.
    (cosi', etai', etat', n') =
      if (cosi < 0)
      then (negate cosi, etat, etai, minus n)
      else (       cosi, etai, etat,       n)

    eta = etai' / etat'

    k = 1 - eta * eta * (1 - cosi' * cosi')

sceneIntersect
  :: [Sphere] -> Material -> Vec3f -> Vec3f -> Vec3f -> Vec3f
  -> (Bool, Material, Vec3f, Vec3f)
sceneIntersect ss m o d p n = (sd < 1000, m', p', n')
  where
    (sd, m', p', n') = go maxValue ss m o d p n

    go :: Float -> [Sphere] -> Material -> Vec3f -> Vec3f -> Vec3f -> Vec3f
       -> (Float, Material, Vec3f, Vec3f)
    go spheresDist []               material _    _   point normal =
      (spheresDist, material, point, normal)
    go spheresDist (sphere:spheres) material orig dir point normal =
      let distI                = 0
          (intersects, distI') = rayIntersect sphere orig dir distI
          material'            = _material sphere
          point'               = orig +. (dir *.. distI')
          normal'              = normalize $ (point' -. _center sphere)
      in if intersects && distI' < spheresDist
         then go distI'      spheres material' orig dir point' normal'
         else go spheresDist spheres material  orig dir point  normal

castRay :: [Sphere] -> [Light] -> Int -> Vec3f -> Vec3f -> Vec3f
castRay spheres lights depth orig dir =
  if depth > maxDepth || not intersects
  then Vec3f 0.2 0.7 0.8  -- background color
  else ((_diffuseColor material' *.. diffuseLightIntensity') *..  _v4f_x (_albedo material')) +.
       ((Vec3f 1 1 1 *.. specularLightIntensity') *.. _v4f_y (_albedo material')) +.
       (reflectColor *.. _v4f_z (_albedo material')) +.
       (refractColor *.. _v4f_w (_albedo material'))
    where
      (intersects, material', point', n')
        = sceneIntersect spheres material orig dir point n
      (diffuseLightIntensity', specularLightIntensity')
        = go diffuseLightIntensity specularLightIntensity lights

      -- XXX: Hardcoded recursion depth.
      maxDepth :: Int
      maxDepth = 4

      material :: Material
      material = makeMaterial

      n :: Vec3f
      n = Vec3f 0 0 0

      point :: Vec3f
      point = Vec3f 0 0 0

      reflectDir = normalize $ reflect dir n'
      refractDir = normalize $ refract dir n' (_refractiveIndex material')

      -- Offset the original point to avoid occlusion by the object itself.
      reflectOrig = if (reflectDir *. n') < 0
                    then point' -. (n' *.. 1e-3)
                    else point' +. (n' *.. 1e-3)

      refractOrig = if (refractDir *. n') < 0
                    then point' -. (n' *.. 1e-3)
                    else point' +. (n' *.. 1e-3)

      reflectColor = castRay spheres lights (depth + 1) reflectOrig reflectDir
      refractColor = castRay spheres lights (depth + 1) refractOrig refractDir

      diffuseLightIntensity :: Float
      diffuseLightIntensity = 0

      specularLightIntensity :: Float
      specularLightIntensity = 0

      go :: Float -> Float -> [Light] -> (Float, Float)
      go dli sli []     = (dli, sli)
      go dli sli (l:ls) =
        let lightDir      = normalize $ (_position l) -. point'
            lightDistance = norm      $ (_position l) -. point'

            -- Check if a point lies in the shadow of a light.
            shadowOrig = if (lightDir *. n') < 0
                         then point' -. (n' *.. 1e-3)
                         else point' +. (n' *.. 1e-3)

            shadowPoint = Vec3f 0 0 0
            shadowN     = Vec3f 0 0 0
            tmpMaterial = makeMaterial

            (intersects', _, shadowPoint', _)
              = sceneIntersect spheres tmpMaterial shadowOrig lightDir shadowPoint shadowN

            refl = minus $ reflect (minus lightDir) n'
            dli' = dli + (_intensity l) * (max 0 (lightDir *. n'))
            sli' = sli + ((max 0 (refl *. dir)) ** (_specularExponent material'))
                 * _intensity l

        in if intersects' && norm (shadowPoint' -. shadowOrig) < lightDistance
           then go dli  sli  ls
           else go dli' sli' ls

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
          let fi    = intToFloat i
              fj    = intToFloat j
              x     =  (2 * (fi + 0.5) / fWidth  - 1) * tan (fov / 2) * fWidth / fHeight
              y     = -(2 * (fj + 0.5) / fHeight - 1) * tan (fov / 2)
              depth = 0
              orig  = Vec3f 0 0 0
              dir   = normalize $ Vec3f x y -1
          in castRay spheres lights depth orig dir

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

    ivory, glass, redRubber, mirror :: Material
    ivory     = Material   1 (Vec4f 0.6 0.3 0.1   0) (Vec3f 0.4 0.4 0.3)   50
    glass     = Material 1.5 (Vec4f   0 0.5 0.1 0.8) (Vec3f 0.6 0.7 0.8)  125
    redRubber = Material   1 (Vec4f 0.9 0.1   0   0) (Vec3f 0.3 0.1 0.1)   10
    mirror    = Material   1 (Vec4f   0  10 0.8   0) (Vec3f   1   1   1) 1425

    spheres :: [Sphere]
    spheres =
      [ Sphere (Vec3f -3    0   -16) 2 ivory
      , Sphere (Vec3f -1.0 -1.5 -12) 2 glass
      , Sphere (Vec3f  1.5 -0.5 -18) 3 redRubber
      , Sphere (Vec3f  7    5   -18) 4 mirror
      ]

    lights :: [Light]
    lights =
      [ Light (Vec3f -20 20  20) 1.5
      , Light (Vec3f  30 50 -25) 1.8
      , Light (Vec3f  30 20  30) 1.7
      ]
