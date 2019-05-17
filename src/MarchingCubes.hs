{-# LANGUAGE BangPatterns #-}
module MarchingCubes where
import           Data.Array.Unboxed  hiding ((!))
import qualified Data.Array.Unboxed  as A
import           Data.Bits           (shiftL)
import qualified Data.Foldable       as F
import           Data.List           (zipWith4, findIndices)
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
import Data.List.Index (imap)

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
marchingCubes voxel mx level = triangles1
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
  r3s = filter (not . UV.null)
        (map (\x -> UV.findIndices (== x) tcase) specialName)
--  unsigned* R3 = whichEqual(tcase, special_name[c], nrow, &nR3);
  setOfTriangles = imap special r3s
  special c r3 = nedge
    where
    nR3 = UV.length r3
    cubeco3 = getBasic1 r3 vivjvk
    values3 = getBasic2 voxel level cubeco3
--    p13 = [8*i + 1 | i <- [0 .. UV.length r3 - 1]]
    p13 = UV.map (\i -> 8*i + 1) (UV.enumFromN 0 nR3)
--    cases3 = UV.map (\j -> vt V.! j - 1) r3
    cases3 = [vt V.! (r3 ! i) | i <- [0 .. UV.length r3 - 1]]
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
          zipWith (+) idx temp
    edges3' = UV.toList $ UV.concat $ map ((V.!) edgesTable2) cases3
    edges3 = vector2matrix edges3' nedge
    edgesp1index = cbind edges3 (UV.toList p13) index3
--    ind3Size = specialIndSizes V.! c
    ind3 = specialInd V.! c


-- unsigned ind3[ind3Size];
-- for(unsigned i=0; i<ind3Size; i++){
--   ind3[i] = (*special_ind[c])[i];
-- }
-- for(unsigned j=0; j<ind3Size; j++){
--   unsigned lwrows;
--   unsigned* wrows = whichEqual(index3, ind3[j], nR3, &lwrows);
--   if(lwrows>0){
--     unsigned lwcols = *(*(special_posSize)[c])[j] + 1;
--     unsigned* wcols = malloc(lwcols * sizeof(unsigned));
--     wcols[0] = nedge;
--     for(unsigned k=1; k<lwcols; k++){
--       wcols[k] = (*(*special_pos[c])[j])[k-1] - 1;
--     }
--     unsigned** ed = subsetMatrix(edgesp1index, wrows, wcols, lwrows, lwcols);
--     size_t* col0ed = malloc(lwrows * sizeof(size_t));
--     for(unsigned i=0; i<lwrows; i++){
--       col0ed[i] = (size_t) ed[i][0];
--     }
--     size_t* col0edrep = repeach(col0ed, lwcols-1, lwrows);
--     free(col0ed);
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
