-- https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing

{-# language OverloadedStrings #-}
{-# language NegativeLiterals #-}

import Data.Bits ((.&.))
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

floatToInt :: Float -> Int
floatToInt = fromEnum

floatToWord8 :: Float -> Word8
floatToWord8 = fromIntegral . fromEnum

newtype Height = Height { _fromHeight :: Int }
newtype Width  = Width  { _fromWidth  :: Int }

-- | Return a list of indexes of a 2d array.
indexes :: Height -> Width -> [(Int,Int)]
indexes (Height height) (Width width) = go 0 0 (width * height) height width
  where
    go i j n h w
      | n <= 0    = []
      | i >= w    =         go 0       (j + 1) n       h w
      | j >= h    =         go (i + 1) 0       n       h w
      | otherwise = (j,i) : go (i + 1) j       (n - 1) h w

-- | Dot (inner) product.
-- Formula:
-- @
-- v1 . v2 = v1x*v2x + v1y*v2y + v1z*v2z
-- @
--
-- The relation with the cosine of the angle between v1 and v2:
-- @
-- v1 . v2 = |v1| |v2| * cos(a)
-- @
--
-- The dot product of two vectors is zero if any of the vectors is null
-- or if the vectors are orthogonal (the angle is 90 degrees).
--
-- http://web.archive.org/web/20170707014314/http://www.lighthouse3d.com/tutorials/maths/inner-product/
infixl 7 *.
(*.) :: Vec3f -> Vec3f -> Float
(Vec3f x1 y1 z1) *. (Vec3f x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Scalar multiplication.
infixl 7 *..
(*..) :: Vec3f -> Float -> Vec3f
(Vec3f x y z) *.. f = Vec3f (x * f) (y * f) (z * f)

-- | Vector addition.
infixl 6 +.
(+.) :: Vec3f -> Vec3f -> Vec3f
(Vec3f x1 y1 z1) +. (Vec3f x2 y2 z2) = Vec3f (x1 + x2) (y1 + y2) (z1 + z2)

-- | Vector subtraction.
infixl 6 -.
(-.) :: Vec3f -> Vec3f -> Vec3f
(Vec3f x1 y1 z1) -. (Vec3f x2 y2 z2) = Vec3f (x1 - x2) (y1 - y2) (z1 - z2)

minus :: Vec3f -> Vec3f
minus v = v *.. -1

newtype Orig = Orig { _fromOrig :: Vec3f }
newtype Dir  = Dir  { _fromDir  :: Vec3f }

-- | Parametric equation for points on the line:
-- @
-- point(t) = p + t * v
-- @
-- where @p@ is a point, @v@ is a vector, and @t@ is a scalar.
--
-- Points on the line can be obtained by varying @t@.
--
-- If @t@ is zero, the equation returns the point @p@.
--
-- A note on terminology:
-- * for a line, @t@ can take any values
-- * for a ray, @t@ must be non-negative.
--
-- The parametric equation can be used with two points @p@ and @p1@.
-- In this case, the vector can be obtained with:
-- @
-- v = p1 - p
-- @
--
-- http://web.archive.org/web/20170707042622/http://www.lighthouse3d.com/tutorials/maths/line-and-rays/
--
-- http://web.archive.org/web/20170707080450/http://www.lighthouse3d.com/tutorials/maths/ray-sphere-intersection/
rayIntersect :: Sphere -> Orig -> Dir -> Float -> (Bool, Float)
rayIntersect (Sphere center radius _) (Orig orig) (Dir dir) t0
  -- If the distance is larger than the radius of a sphere,
  -- there is no intersection.
  | d2 > radius * radius = (False, t0)
  | t1 < 0 && t2 < 0     = (False, t2)
  | t1 < 0               = (True, t2)
  | otherwise            = (True, t1)
  where
    -- Find the vector from the origin to the center of a sphere.
    l :: Vec3f
    l = center -. orig

    tca :: Float
    tca = l *. dir

    -- Squared distance between the center of a sphere and the projection point
    -- on the ray.
    d2 :: Float
    d2 = l *. l - tca * tca

    -- Distance from the projection of the center of a sphere and the
    -- intersection point.
    --
    -- A right triangle is formed by the projection point, the intersection point,
    -- and the center.  The Pythagoras' theorem can be used to find one of the
    -- following:
    -- * the radius (the distance between the center and the intersection point)
    -- * the distance between the center and the projection point on the ray
    -- * the distance between the projection point and the intersection point on
    --   the ray.
    thc :: Float
    thc = sqrt $ radius * radius - d2

    t1 = tca - thc
    t2 = tca + thc

reflect :: Dir -> Normal -> Vec3f
reflect (Dir i) (Normal n) = i -. ((n *.. 2) *.. (i *. n))

-- | Snell's law, see <https://en.wikipedia.org/wiki/Snell%27s_law>.
refract :: Dir -> Normal -> Float -> Vec3f
refract (Dir i) (Normal n) refractiveIndex =
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

newtype Point  = Point  { _fromPoint  :: Vec3f }
newtype Normal = Normal { _fromNormal :: Vec3f }

sceneIntersect
  :: [Sphere] -> Material -> Orig -> Dir -> Point -> Normal
  -> (Bool, Material, Point, Normal)
sceneIntersect spheres material orig dir point normal =
  ((min spheresDist checkerboardDist) < 1000, material'', point'', normal'')
  where
    (spheresDist, material', point', normal')
      = go maxValue spheres material orig dir point normal

    go :: Float -> [Sphere] -> Material -> Orig -> Dir -> Point -> Normal
       -> (Float, Material, Point, Normal)
    go sd []     m _ _ p n = (sd, m, p, n)
    go sd (s:ss) m o d p n =
      let di                = 0
          (intersects, di') = rayIntersect s o d di
          m'                = _material s
          p'                = Point $ (_fromOrig o) +. ((_fromDir d) *.. di')
          n'                = Normal $ normalize $ (_fromPoint p' -. _center s)
      in if intersects && di' < sd
         then go di' ss m' o d p' n'
         else go sd  ss m  o d p  n

    (checkerboardDist, material'', point'', normal'') =
      if abs (_v3f_y (_fromDir dir)) > 1e-3
      then
        -- Checkerboard plane equation: 'y = -4'.
        let d = -(_v3f_y (_fromOrig orig) + 4) / (_v3f_y $ _fromDir dir)
            p@(Point (Vec3f x _ z)) = Point $ (_fromOrig orig) +. ((_fromDir dir) *.. d)
        in if (d > 0 && abs x < 10 && z < -10 && z > -30 && d < spheresDist)
           then
             let n = Normal $ Vec3f 0 1 0
                 diffuseColor =
                   if ((floatToInt (0.5 * x + 1000) + floatToInt (0.5 * z)) .&. 1) /= 0
                   then Vec3f 1   1   1
                   else Vec3f 1 0.7 0.3
                 diffuseColor' = diffuseColor *.. 0.3
                 m = material' { _diffuseColor = diffuseColor' }
             in (d, m, p, n)
           else (maxValue, material', point', normal')
      else (maxValue, material', point', normal')

castRay :: [Sphere] -> [Light] -> Int -> Orig -> Dir -> Vec3f
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

      n :: Normal
      n = Normal $ Vec3f 0 0 0

      point :: Point
      point = Point $ Vec3f 0 0 0

      reflectDir = Dir $ normalize $ reflect dir n'
      refractDir = Dir $ normalize $ refract dir n' (_refractiveIndex material')

      -- Offset the original point to avoid occlusion by the object itself.
      reflectOrig = Orig $
        if (_fromDir reflectDir *. _fromNormal n') < 0
        then _fromPoint point' -. (_fromNormal n' *.. 1e-3)
        else _fromPoint point' +. (_fromNormal n' *.. 1e-3)

      refractOrig = Orig $
        if (_fromDir refractDir *. _fromNormal n') < 0
        then _fromPoint point' -. (_fromNormal n' *.. 1e-3)
        else _fromPoint point' +. (_fromNormal n' *.. 1e-3)

      reflectColor = castRay spheres lights (depth + 1) reflectOrig reflectDir
      refractColor = castRay spheres lights (depth + 1) refractOrig refractDir

      diffuseLightIntensity :: Float
      diffuseLightIntensity = 0

      specularLightIntensity :: Float
      specularLightIntensity = 0

      go :: Float -> Float -> [Light] -> (Float, Float)
      go dli sli []     = (dli, sli)
      go dli sli (l:ls) =
        let lightDir      = Dir $ normalize $ (_position l) -. (_fromPoint point')
            lightDistance =       norm      $ (_position l) -. (_fromPoint point')

            -- Check if a point lies in the shadow of a light.
            shadowOrig = Orig $
              if (_fromDir lightDir *. _fromNormal n') < 0
              then _fromPoint point' -. (_fromNormal n' *.. 1e-3)
              else _fromPoint point' +. (_fromNormal n' *.. 1e-3)

            shadowPoint = Point $ Vec3f 0 0 0
            shadowN     = Normal $ Vec3f 0 0 0
            tmpMaterial = makeMaterial

            (intersects', _, shadowPoint', _)
              = sceneIntersect spheres tmpMaterial shadowOrig lightDir shadowPoint shadowN

            refl = minus $ reflect (Dir $ minus $ _fromDir lightDir) n'
            dli' = dli + (_intensity l) * (max 0 (_fromDir lightDir *. _fromNormal n'))
            sli' = sli + ((max 0 (refl *. _fromDir dir)) ** (_specularExponent material'))
                 * _intensity l

        in if intersects' && norm (_fromPoint shadowPoint' -. _fromOrig shadowOrig) < lightDistance
           then go dli  sli  ls
           else go dli' sli' ls

normalize :: Vec3f -> Vec3f
normalize v = v *.. (l  / norm v)
  where
    l = 1

norm :: Vec3f -> Float
norm (Vec3f x y z) = sqrt $ x * x + y * y + z * z

-- | Write an image to disk.
render :: FilePath -> Width -> Height -> [Sphere] -> [Light] -> IO ()
render file width height spheres lights =
  BSC.writeFile file (header <> image)
  where
    -- Field of view.
    fov :: Float
    fov = pi / 2

    fWidth :: Float
    fWidth = intToFloat $ _fromWidth width

    fHeight :: Float
    fHeight = intToFloat $ _fromHeight height

    framebuffer :: Vector Vec3f
    framebuffer =
      V.fromList $
        flip fmap (indexes height width) $ \(j,i) ->
          let fi    = intToFloat i
              fj    = intToFloat j
              x     =  (2 * (fi + 0.5) / fWidth  - 1) * tan (fov / 2) * fWidth / fHeight
              y     = -(2 * (fj + 0.5) / fHeight - 1) * tan (fov / 2)
              depth = 0
              orig  = Orig $ Vec3f 0 0 0
              dir   = Dir $ normalize $ Vec3f x y -1
          in castRay spheres lights depth orig dir

    ppmMaxVal :: Int
    ppmMaxVal = 255

    header :: ByteString
    header =
      BSC.concat
        [ "P6\n"
        , BSC.pack (show $ _fromWidth width)
        , " "
        , BSC.pack (show $ _fromHeight height)
        , "\n"
        , BSC.pack (show ppmMaxVal)
        , "\n"
        ]

    image :: ByteString
    image =
      BS.pack $
        -- XXX: Take advantage of the Haskell 'Vec3f' representation.
        -- In the original C++ code, 'Vec3f' is just an array of size 3.
        flip fmap (indexes iHeight iWidth) $ \(i,j) ->
          let c@(Vec3f x y z) = framebuffer V.! i
              fmax            = max x $ max y z
              c'              = c *.. (1 / fmax)
          in if (fmax > 1)
             then floatToWord8 $ (intToFloat ppmMaxVal) * (max 0 (min 1 (c' !!. j)))
             else floatToWord8 $ (intToFloat ppmMaxVal) * (max 0 (min 1 (c  !!. j)))
      where
        iHeight = Height $ _fromWidth width * _fromHeight height
        iWidth  = Width 3

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
        (Just width, Just height)
          -> render file (Width width) (Height height) spheres lights
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
