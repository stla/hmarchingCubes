module Mesh where
import           Data.List.Split     (chunksOf)
import           Data.Map.Strict     (insertLookupWithKey)
import qualified Data.Map.Strict     as M
import           Data.Matrix         (Matrix, toLists, nrows)
import           Data.Tuple.Extra    (curry3, first, second, thd3, (***))
import           Data.Vector.Unboxed (Unbox, Vector, fromList, (!))
import           MarchingCubes

type Mesh a = (Vector a, [[Int]])

-- matrix2vector :: Unbox a => Matrix a -> Vector (a,a,a)
-- matrix2vector mtrx = fromList listOfTriples
--   where
--   mtrxAsList = toLists mtrx
--   listOfTriples = map (\row -> (row !! 0, row !! 1, row !! 2)) mtrxAsList
matrix2listOfTriples :: Unbox a => Matrix a -> [(a,a,a)]
matrix2listOfTriples mtrx =
  map (\row -> (row !! 0, row !! 1, row !! 2)) (toLists mtrx)

uniqueWithIndices :: (Unbox a, Ord a) => [a] -> (Vector a, Vector Int)
uniqueWithIndices list = (fromList *** fromList) $ go 0 M.empty list
  where
    go _ _ [] = ([],[])
    go i m (x:xs) = case insertLookupWithKey (curry3 thd3) x i m of
        (Nothing, m') -> first (x:) (second (i:) (go (i+1) m' xs))
        (Just j , m') ->             second (j:) (go i     m' xs)

undupMesh :: (Unbox a, Ord a) => ([a], [[Int]]) -> Mesh a
undupMesh (vertices, faces) = (vertices', faces')
  where
  (vertices', idx) = uniqueWithIndices vertices
  faces' = map (map (idx !)) faces

makeMesh :: (Unbox a, RealFloat a, Ord a) => Voxel a -> a -> Mesh (a,a,a)
makeMesh voxel level = undupMesh (vertices, faces)
  where
  mtrx = marchingCubes voxel level
  vertices = matrix2listOfTriples mtrx
  faces = chunksOf 3 [0 .. nrows mtrx - 1]
