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
           i <- [0 .. nR-1], j <- [0 .. edgeslengths ! i]]
  epoints = map (\j -> edgePoints V.! (j-1)) edges
  x1 = map (! 1) epoints
  x2 = map (! 2) epoints
  points = getPoints cubeco values p1rep x1 x2
  triangles = calPoints points

  -- double** points = GetPoints(cubeco, values, p1rep, x1, x2, totalLength);
  -- double** triangles = CalPoints(points, totalLength);
