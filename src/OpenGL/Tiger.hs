module OpenGL.Tiger
  (main)
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Matrix                       hiding ((!))
import qualified Data.Matrix                       as M
import           Data.Vector.Unboxed               (Vector, (!))
import qualified Data.Vector.Unboxed               as V
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           Linear.Metric
import           Linear.V4
import           Linear.Vector
import           MarchingCubes
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf

white,black,navy :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
navy = Color4 0 0 0.5 1

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

type F = Double

parameterization :: XYZ F -> V4 F
parameterization (u, v, w) = V4 x y z w
  where
    x = cos u * (2 + 0.5 * cos w)
    y = sin u * (2 + 0.5 * cos w)
    z = cos v * (2 + 0.5 * sin w)
    w = sin v * (2 + (0.5 * sin w))

a :: V4 F
a = V4 1 1 1 1

offset :: F
offset = 2

x0 :: V4 F
x0 = V4 x x x x
  where
    x = offset / 4

nrml :: V4 F
nrml = signorm a

fun :: XYZ F -> F
fun uvw = (parameterization uvw ^-^ x0) `dot` nrml

voxel :: Voxel F
voxel = makeVoxel fun ((0,2*pi),(0,2*pi),(0,2*pi)) (50, 50, 50)

mesh0 :: Mesh F
mesh0 = makeMesh voxel 0

vs0 :: Vector (XYZ F)
vs0 = fst $ fst mesh0

faces :: [[Int]]
faces = snd $ fst mesh0

vs :: Vector (V4 F)
vs = V.map parameterization vs0

v4toColumnMatrix :: V4 F -> Matrix F
v4toColumnMatrix v = M.fromList 4 1 (V.toList v)

rot :: Matrix F
rot = elementwiseUnsafe (-) vvT'' id3
  where
    e4 = V4 0 0 0 1
    v = nrml ^+^ e4
    lambda = 2 / quadrance v
    vAsMatrix = v4toColumnMatrix v
    vvT = multStd2 vAsMatrix (M.transpose vAsMatrix)
    vvT' = submatrix 1 3 1 4 vvT
    vvT'' = scaleMatrix lambda vvT'
    id3 = identity 3

columnMatrix2XYZ :: Matrix F -> XYZ F
columnMatrix2XYZ colmat = (x, y, z)
  where
    x = getElem 1 1 colmat
    y = getElem 2 1 colmat
    z = getElem 3 1 colmat

vs' :: Vector (XYZ F)
vs' = V.map f vs
  where
    f v = columnMatrix2XYZ $ multStd2 rot (v4toColumnMatrix v)

gradient :: V4 F -> V4 F
gradient v = V4 (x * fxy) (y * fxy) (z * fzt) (t * fzt)
  where
    V4 x y z t = v
    fxy = 1 - 2 / sqrt(x*x+y*y)
    fzt = 1 - 2 / sqrt(z*z+t*t)

normals0 :: Vector (V4 F)
normals0 = V.map gradient vs

mprojection :: V4 F -> V4 F
mprojection v = (lambda *^ a) ^-^ v
  where
    lambda = (a `dot` v - offset) / quadrance a

normals :: Vector (XYZ F)
normals = V.map f normals0
  where
    f n = columnMatrix2XYZ $ multStd2 rot (v4toColumnMatrix $ mprojection n)

triangle :: [Int] -> ((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F))
triangle face =
  ( (vs' ! i, vs' ! j, vs' ! k)
  , (normals ! i, normals ! j, normals ! k))
  where
    i = face !! 0
    j = face !! 2
    k = face !! 1

triangles :: [((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F))]
triangles = map triangle faces
