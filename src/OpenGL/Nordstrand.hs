module OpenGL.Nordstrand
  (main)
  where
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (when)
import qualified Data.ByteString                   as B
import           Data.IORef
import           Data.Vector.Unboxed               (Vector, (!))
import qualified Data.Vector.Unboxed               as V
import           Graphics.Rendering.OpenGL.Capture (capturePPM)
import           Graphics.Rendering.OpenGL.GL
import           Graphics.UI.GLUT
import           MarchingCubes
import           MultiPol.MultiPol
import           System.Directory                  (doesDirectoryExist)
import           System.IO.Unsafe
import           Text.Printf

white,black,navy,forestgreen,cyan,yellow :: Color4 GLfloat
white = Color4 1 1 1 1
black = Color4 0 0 0 1
navy = Color4 0 0 0.5 1
forestgreen = Color4 0.13 0.55 0.13 1
cyan = Color4 0 1 1 1
yellow = Color4 1 1 0 1

data Context = Context
    {
      contextRot1 :: IORef GLfloat
    , contextRot2 :: IORef GLfloat
    , contextRot3 :: IORef GLfloat
    , contextZoom :: IORef Double
    }

type F = Float

fun :: XYZ F -> F
fun (ρ, θ, ϕ) =
  25*(x3*(y+z)+y3*(x+z)+z3*(x+y)) + 50*(x2*y2+x2*z2+y2*z2) -
  125*(x2*y*z+y2*x*z+z2*x*y) + 60*x*y*z - 4*(x*y+x*z+y*z)
  where
    x = ρ * cos θ * sin ϕ
    y = ρ * sin θ * sin ϕ
    z = ρ * cos ϕ
    x2 = x*x
    y2 = y*y
    z2 = z*z
    x3 = x*x2
    y3 = y*y2
    z3 = z*z2

pol :: Polynomial F
pol = (M (Monomial 25 (0,0,0)) :*:
  fromListOfMonomials [ Monomial 1 (3,1,0)
                      , Monomial 1 (3,0,1)
                      , Monomial 1 (1,3,0)
                      , Monomial 1 (0,3,1)
                      , Monomial 1 (1,0,3)
                      , Monomial 1 (0,1,3)]) :+:
  (M (Monomial 50 (0,0,0)) :*:
  fromListOfMonomials [ Monomial 1 (2,2,0)
                      , Monomial 1 (2,0,2)
                      , Monomial 1 (0,2,2)]) :+:
  (M (Monomial (-125) (0,0,0)) :*:
  fromListOfMonomials [ Monomial 1 (2,1,1)
                      , Monomial 1 (1,2,1)
                      , Monomial 1 (1,1,2)]) :+:
  M (Monomial 60 (1,1,1)) :+:
  (M (Monomial (-4) (0,0,0)) :*:
  fromListOfMonomials [ Monomial 1 (1,1,0)
                      , Monomial 1 (1,0,1)
                      , Monomial 1 (0,1,1)])

fundx :: XYZ F -> F
fundx = evalPoly $ derivPoly pol 'x'

fundy :: XYZ F -> F
fundy = evalPoly $ derivPoly pol 'y'

fundz :: XYZ F -> F
fundz = evalPoly $ derivPoly pol 'z'

gradient :: XYZ F -> XYZ F
gradient xyz = (-x/l, -y/l, -z/l)
  where
    x = fundx xyz
    y = fundy xyz
    z = fundz xyz
    l = sqrt(x*x + y*y + z*z)

voxel :: Voxel F
voxel = makeVoxel fun ((0,1),(0,2*pi),(0,pi)) (100, 100, 100)

nordstrandQuartic :: Mesh F
nordstrandQuartic = makeMesh voxel 0

vertices :: Vector (XYZ F)
vertices = V.map (\(ρ,θ,ϕ) -> (ρ * cos θ * sin ϕ, ρ * sin θ * sin ϕ, ρ * cos ϕ))
                 (fst $ fst nordstrandQuartic)

faces :: [[Int]]
faces = snd $ fst nordstrandQuartic

normals :: Vector (XYZ F)
normals = V.map gradient vertices

triangle :: [Int] -> ((XYZ F, XYZ F, XYZ F), (XYZ F, XYZ F, XYZ F))
triangle face =
  ( (vertices ! i, vertices ! j, vertices ! k)
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
      materialDiffuse Back $= forestgreen
      vertex (toVertex v1)
      normal (toNormal n2)
      materialDiffuse Front $= navy
      materialDiffuse Back $= forestgreen
      vertex (toVertex v2)
      normal (toNormal n3)
      materialDiffuse Front $= navy
      materialDiffuse Back $= forestgreen
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
  lookAt (Vertex3 0 (-3+zoom) 0) (Vertex3 0 0 0) (Vector3 0 0 1)
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
    a <- get anim
    snapshot <- get snapshots
    s <- get save
    when a $ do
      d <- get delay
      when (s && ppmExists && snapshot < 360) $ do
        let ppm = printf "ppm/pic%04d.ppm" snapshot
        (>>=) capturePPM (B.writeFile ppm)
        print snapshot
        snapshots $~! (+1)
      rot3 $~! (+1)
      _ <- threadDelay d
      postRedisplay Nothing
    return ()

main :: IO ()
main = do
  _ <- getArgsAndInitialize
  _ <- createWindow "Nordstrand quartic"
  windowSize $= Size 500 500
  initialDisplayMode $= [RGBAMode, DoubleBuffered, WithDepthBuffer]
  clearColor $= white
  materialAmbient FrontAndBack $= black
  materialShininess FrontAndBack $= 100
  materialSpecular Front $= cyan
  materialSpecular Back $= yellow
  lighting $= Enabled
  lightModelTwoSide $= Enabled
  light (Light 0) $= Enabled
  position (Light 0) $= Vertex4 0 (-100) 0 1
  ambient (Light 0) $= black
  diffuse (Light 0) $= white
  specular (Light 0) $= white
  depthFunc $= Just Less
  shadeModel $= Smooth
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
  putStrLn "*** Nordstrand quartic ***\n\
        \    To quit, press q.\n\
        \    Scene rotation:\n\
        \        e, r, t, y, u, i\n\
        \    Zoom: l, m\n\
        \    Animation: a\n\
        \    Animation speed: o, p\n\
        \    Save animation: s\n\
        \"
  mainLoop
