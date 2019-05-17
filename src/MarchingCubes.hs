{-# LANGUAGE BangPatterns #-}
module MarchingCubes where
import           Data.Array.Unboxed  hiding ((!))
import qualified Data.Array.Unboxed  as A
import           Data.Bits           (shiftL)
import qualified Data.Foldable       as F
import           Data.List           (elemIndices, findIndices, zipWith4)
import           Data.List.Index     (imap)
import           Data.Matrix         hiding ((!))
import qualified Data.Matrix         as M
import           Data.Maybe          (isJust, fromJust)
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
marchingCubes voxel mx level = maybe triangles1 (triangles1 <->) triangles2
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
  triangles1 = calPoints points
  -- special cases
  r3s = filter (not . UV.null)
        (map (\x -> UV.findIndices (== x) tcase) specialName)
  setOfTriangles = map fromJust $ filter isJust (imap special r3s)
  special c r3 = if null newtriangles
    then
      Nothing
    else
      Just $ foldr (<->) (head newtriangles) (tail newtriangles)
    where
    nR3 = UV.length r3
    cubeco3 = getBasic1 r3 vivjvk
    values3 = getBasic2 voxel level cubeco3
--    p13 = [8*i + 1 | i <- [0 .. UV.length r3 - 1]]
    p13 = UV.map (\i -> 8*i + 1) (UV.enumFromN 0 nR3)
--    cases3 = UV.map (\j -> vt V.! j - 1) r3
    cases3 = [vt V.! (r3 ! i) - 1 | i <- [0 .. nR3 - 1]]
    nedge = specialNedge ! c
    faces3 = UV.concat $ map ((V.!) facesTable) cases3
    index3 = case c of
      0 -> facesNo7 faces3 p13 values3 nR3 1
      1 -> faces7 faces3 p13 values3 nR3 1
      _ -> index3'
      where
      nface = specialNface ! c
      facelast = jthColumn faces3 nface (nface - 1)
      index3' = loop 0 (faces7 facelast p13 values3 nR3 nface)
      loop j !idx
        | j == nface-1 = idx
        | otherwise =
          let facej = jthColumn faces3 nface j in
          let temp = facesNo7 facej p13 values3 nR3 (j+1) in
          loop (j+1) (zipWith (+) idx temp)
    edges3' = UV.toList $ UV.concat $ map ((V.!) edgesTable2) cases3
    edges3 = vector2matrix edges3' nedge
    edgesp1index = cbind edges3 (UV.toList p13) index3
--    ind3Size = specialIndSizes V.! c
    ind3 = specialInd V.! c
    newtriangles = map fromJust (filter isJust (map f [0 .. UV.length ind3 - 1]))
    f j = triangles3
      where
      wrows = elemIndices (ind3 ! j) index3
      triangles3 = if null wrows
        then
          Nothing
        else
          Just $ calPoints points3
        where
          -- ce serait mieux de faire -1 dans Tables.hs
          wcols = UV.cons nedge (UV.map (subtract 1) (UV.init ((specialPos V.! c) V.! j)))
          ed = subMatrix edgesp1index wrows wcols
          col0ed = V.toList $ getCol 1 ed
          col0edrep = F.toList $ replicateEach' col0ed (UV.length wcols -1)
          edge1 = matrix2listMinusFirstColumn ed
          epoints' = map (\j -> edgePoints V.! (j-1)) edge1
          x1' = map (! 1) epoints'
          x2' = map (! 2) epoints'
          points3 = getPoints cubeco3 values3 col0edrep x1' x2'
  triangles2 = if null setOfTriangles
    then
      Nothing
    else
      Just $ foldr (<->) (head setOfTriangles) (tail setOfTriangles)

-- for(unsigned j=0; j<ind3Size; j++){
--     unsigned* edge1 = matrix2vectorMinusFirstColumn(ed, lwrows, lwcols);
--     freeMatrix_u(ed, lwrows);
--     unsigned totalLength3 = lwrows*(lwcols-1);
--     unsigned* xx1 = malloc(totalLength3 * sizeof(unsigned));
--     unsigned* xx2 = malloc(totalLength3 * sizeof(unsigned));
--     for(unsigned i=0; i<totalLength3; i++){
--         unsigned* EPi = EdgePoints[edge1[i]-1];
--         xx1[i] = EPi[1]; xx2[i] = EPi[2];
--     }
--     double** points3 = GetPoints(cubeco3, values3, col0edrep, xx1,
--                        xx2, (size_t) totalLength3);
--     double** triangles3 = CalPoints(points3, totalLength3);
--     triangles = realloc(triangles,
--                    (*ntriangles + totalLength3)*sizeof(*triangles));
--     for(size_t i = *ntriangles; i < *ntriangles + totalLength3; i++){
--       triangles[i] = malloc(3 * sizeof(double));
--       for(short j=0; j<3; j++){
--         triangles[i][j] = triangles3[i - *ntriangles][j];
--       }
--     }
--     *ntriangles += totalLength3;
--   }
-- } /* end loop for(unsigned j=0; j<ind3Size; j++) */



ftest :: (Double,Double,Double) -> Double
ftest (x,y,z) = x*x + y*y + z*z - 1

voxel = makeVoxel ftest ((-1,1),(-1,1),(-1,1)) (5,5,5)

mc = marchingCubes voxel 5 0

fEgg :: (Double,Double,Double) -> Double
fEgg (x,y,z) =
  - sq(cos x * sin y + cos y * sin z + cos z * sin x) +
    0.05-exp(100.0*(x*x/64+y*y/64 + z*z/(1.6*64)*exp(-0.4*z/8) - 1))
  where
  sq a = a*a

voxel' = makeVoxel fEgg ((-7.6,7.6),(-7.6,7.6),(-8,14))
                  (5, 5, 5)

mc' = marchingCubes voxel' 50000000 0
