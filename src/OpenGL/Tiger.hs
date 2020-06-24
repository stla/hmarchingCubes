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
import           Graphics.Rendering.OpenGL.GL      hiding (Matrix)
import           Graphics.UI.GLUT                  hiding (Matrix)
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
parameterization (u, v, w) = V4 x y z t
  where
    x = cos u * (2 + 0.5 * cos w)
    y = sin u * (2 + 0.5 * cos w)
    z = cos v * (2 + 0.5 * sin w)
    t = sin v * (2 + (0.5 * sin w))

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
v4toColumnMatrix v = M.fromList 4 1 [x,y,z,w]
  where
    V4 x y z w = v

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

projection :: V4 F -> V4 F
projection v = v ^-^ (lambda *^ a)
  where
    lambda = (a `dot` v - offset) / quadrance a

normals :: Vector (XYZ F)
normals = V.map f normals0
  where
    f n = columnMatrix2XYZ $ multStd2 rot (v4toColumnMatrix $ projection n)

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

display :: Context -> DisplayCallback
display context = do
  clear [ColorBuffer, DepthBuffer]
  r1 <- get (contextRot1 context)
  r2 <- get (contextRot2 context)
  r3 <- get (contextRot3 context)
  zoom <- get (contextZoom context)
  (_, size) <- get viewport
  loadIdentity
  resize zoom size
  rotate r1 $ Vector3 1 0 0
  rotate r2 $ Vector3 0 1 0
  rotate r3 $ Vector3 0 0 1
  renderPrimitive Triangles $
    mapM_ drawTriangle triangles
  swapBuffers
  where
    drawTriangle ((v1,v2,v3),(n1,n2,n3)) = do
      normal (toNormal n1)
      materialDiffuse Front $= navy
      vertex (toVertex v1)
      normal (toNormal n2)
      materialDiffuse Front $= navy
      vertex (toVertex v2)
      normal (toNormal n3)
      materialDiffuse Front $= navy
      vertex (toVertex v3)
      where
        toNormal (x,y,z) = Normal3 x y z
        toVertex (x,y,z) = Vertex3 x y z

resize :: Double -> Size -> IO ()
resize zoom s@(Size w h) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45.0 (realToFrac w / realToFrac h) 1.0 100.0
  lookAt (Vertex3 0 (-10+zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
  matrixMode $= Modelview 0

keyboard :: IORef GLfloat -> IORef GLfloat -> IORef GLfloat -- rotations
         -> IORef Double -- zoom
         -> IORef Bool -- animation
         -> IORef Int -- animation delay
         -> IORef Bool -- save animation
         -> KeyboardCallback
keyboard rot1 rot2 rot3 zoom anim delay save c _ = do
  case c of
    'e' -> rot1 $~! subtract 2
    'r' -> rot1 $~! (+ 2)
    't' -> rot2 $~! subtract 2
    'y' -> rot2 $~! (+ 2)
    'u' -> rot3 $~! subtract 2
    'i' -> rot3 $~! (+ 2)
    'm' -> zoom $~! (+ 1)
    'l' -> zoom $~! subtract 1
    'a' -> anim $~! not
    'o' -> delay $~! (+10000)
    'p' -> delay $~! (\d -> if d==0 then 0 else d-10000)
    's' -> save $~! not
    'q' -> leaveMainLoop
    _   -> return ()
  postRedisplay Nothing

ppmExists :: Bool
{-# NOINLINE ppmExists #-}
ppmExists = unsafePerformIO $ doesDirectoryExist "./ppm"

idle :: IORef Bool -> IORef Int -> IORef Bool -> IORef Int -> IORef GLfloat
     -> IdleCallback
idle anim delay save snapshots rot3 = do
    anm <- get anim
    snapshot <- get snapshots
    s <- get save
    when anm $ do
      d <- get delay
      when (s && ppmExists && snapshot < 360) $ do
        let ppm = printf "ppm/pic%04d.ppm" snapshot
        (>>=) capturePPM (B.writeFile ppm)
        print snapshot
        snapshots $~! (+1)
      rot3 $~! (+1)
      _ <- threadDelay d
      postRedisplay Nothing
--    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Tiger"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient Front $= black
  materialShininess Front $= 100
  lighting $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-100) 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
  cullFace $= Just Back
  rot1 <- newIORef 0.0
  rot2 <- newIORef 0.0
  rot3 <- newIORef 0.0
  zoom <- newIORef 0.0
  displayCallback $= display Context {contextRot1 = rot1,
                                      contextRot2 = rot2,
                                      contextRot3 = rot3,
                                      contextZoom = zoom}
  reshapeCallback $= Just (resize 0)
  anim <- newIORef False
  delay <- newIORef 0
  save <- newIORef False
  snapshots <- newIORef 0
  keyboardCallback $= Just (keyboard rot1 rot2 rot3 zoom anim delay save)
  idleCallback $= Just (idle anim delay save snapshots rot3)
  putStrLn "*** Tiger ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
