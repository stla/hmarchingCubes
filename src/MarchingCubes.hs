{-# LANGUAGE BangPatterns #-}
module MarchingCubes where
import           Data.Array.Unboxed  hiding ((!))
import qualified Data.Array.Unboxed  as A
import           Data.Bits           (shiftL)
import qualified Data.Foldable       as F
import           Data.List           (zipWith4)
import           Data.Matrix         hiding ((!))
import qualified Data.Matrix         as M
import           Data.Sequence       (Seq, (|>))
import qualified Data.Sequence       as S
import qualified Data.Vector         as V
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as UV
import           Internals
import           Tables
import           Utils

makeVoxel :: ((Double,Double,Double) -> Double)
          -> ((Double,Double),(Double,Double),(Double,Double))
          -> (Int, Int, Int)
          -> Array (Int,Int,Int) Double
makeVoxel fun ((xm,xM),(ym,yM),(zm,zM)) (nx, ny, nz) =
  listArray ((0,0,0), (nx-1,ny-1,nz-1)) values
  where
  x_ = [xm + (xM-xm) * fracx i | i <- [0..nx-1]]
  fracx p = realToFrac p / (realToFrac nx - 1)
  y_ = [ym + (yM-ym) * fracy i | i <- [0..ny-1]]
  fracy p = realToFrac p / (realToFrac ny - 1)
  z_ = [zm + (zM-zm) * fracz i | i <- [0..nz-1]]
  fracz p = realToFrac p / (realToFrac nz - 1)
  values = map fun [(x,y,z) | x <- x_, y <- y_, z <- z_]

marchingCubes :: Array (Int,Int,Int) Double -> Double -> Double -> Matrix Double
marchingCubes voxel mx level = triangles
  where
  (_, (nx',ny',nz')) = bounds voxel
  ijkt = levCells voxel level mx
  vt = getRow 4 ijkt
  tcase = getTcase vt
  r = getR tcase
  nR = UV.length r
  ijk = submatrix 1 3 1 (ncols ijkt) ijkt
  vivjvk = M.transpose ijk
  cubeco = getBasic1 r vivjvk
  values = getBasic2 voxel level cubeco
  p1 = [8*i + 1 | i <- [0 .. UV.length r -1]]
--  p1 = UV.map (\i -> 8*i + 1) UV.enumFromN 0 nR
  cases = UV.map (\j -> vt V.! j - 1) r
  edgeslengths = UV.map (\j -> edgesLengths UV.! j) cases
  p1rep = F.toList $ replicateEach p1 (UV.toList edgeslengths)
  edges = [(edgesTable V.! (cases ! i)) ! j |
           i <- [0 .. nR-1], j <- [0 .. edgeslengths ! i - 1]]
  epoints = map (\j -> edgePoints V.! (j-1)) edges
  x1 = map (! 1) epoints
  x2 = map (! 2) epoints
  points = getPoints cubeco values p1rep x1 x2
  triangles = calPoints points

  -- double** points = GetPoints(cubeco, values, p1rep, x1, x2, totalLength);
  -- double** triangles = CalPoints(points, totalLength);

ftest :: (Double,Double,Double) -> Double
ftest (x,y,z) = x*x + y*y + z*z - 1

voxel = makeVoxel ftest ((-1,1),(-1,1),(-1,1)) (5,5,5)

mc = marchingCubes voxel 5 0
