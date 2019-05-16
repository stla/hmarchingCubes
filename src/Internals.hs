{-# LANGUAGE BangPatterns #-}
module Internals where
import           Data.Array.Unboxed  hiding ((!))
import qualified Data.Array.Unboxed  as A
import           Data.Matrix         hiding ((!))
import qualified Data.Matrix         as M
import           Data.Sequence       (Seq, (><), (|>))
import qualified Data.Sequence       as S
import qualified Data.Vector         as V
import           Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as UV
import Tables
import Utils
import Data.Bits (shiftL)

facesNo7 :: Vector Int -> Vector Int -> Vector Double -> Int -> Int -> [Int]
facesNo7 faces p1 values l j = map fun [0 .. l-1]
  where
  fun i = if temp == 1 then shiftL 1 (j-1) else 0
    where
    f = faces ! i
    e = facePoints V.! (f-1)
    e1 = e ! 1
    e2 = e ! 2
    e3 = e ! 3
    e4 = e ! 4
    p = p1 ! i - 2
    a = values ! (p+e1)
    b = values ! (p+e2)
    c = values ! (p+e3)
    d = values ! (p+e4)
    temp = (if f>0 then 1::Int else -1) * (if a*b-c*d > 0 then 1 else -1)

faces7 :: Vector Int -> Vector Int -> Vector Double -> Int -> Int -> [Int]
faces7 faces p1 values l j = map fun [0 .. l-1]
  where
  fun i = if temp == 1 then shiftL 1 (j-1) else 0
    where
    p = (p1 ! i) - 1
    a0 = values ! p
    b0 = values ! (p+3)
    c0 = values ! (p+2)
    d0 = values ! (p+1)
    a1 = values ! (p+4)
    b1 = values ! (p+7)
    c1 = values ! (p+6)
    d1 = values ! (p+5)
    a = (a1 - a0) * (c1 - c0) - (b1 - b0) * (d1 - d0)
    b = c0 * (a1 - a0) + a0 * (c1 - c0) - d0 * (b1 - b0) - b0 * (d1 - d0)
    c = a0 * c0 - b0 * d0
    tmax = - b / 2 / a
    mxmm = a * tmax*tmax + b * tmax + c
    mxmm' = if isNaN mxmm then -1 else mxmm
    cond1 = a < 0
    cond2 = tmax > 0
    cond3 = tmax < 1
    cond4 = mxmm' > 0
    totalcond = cond1 && cond2 && cond3 && cond4
    temp = (if faces!i > 0 then 1::Int else -1) * (if totalcond then 1 else -1)

faceType :: Matrix Double -> Double -> Double -> Matrix Int
faceType mtrx level mx = elementwiseUnsafe (+) sum1 sum2
  where
  lm = levelMatrix mtrx level (level < mx)
  m = nrows mtrx
  n = ncols mtrx
  minorMat = minorMatrix m n lm
  sminorMat2 = scaleMatrix 2 (minorMatrix 1 n lm)
  sminorMat4 = scaleMatrix 4 (minorMatrix 1 1 lm)
  sminorMat8 = scaleMatrix 8 (minorMatrix m 1 lm)
  sum1 = elementwiseUnsafe (+) minorMat sminorMat2
  sum2 = elementwiseUnsafe (+) sminorMat4 sminorMat8

levCells :: Array (Int,Int,Int) Double -> Double -> Double -> Matrix Int
levCells a level mx = out
  where
  bottomTypes = faceType (arrayToMatrix a 0) level mx
  (_, (nx',ny',nz')) = bounds a
  -- nx = nx' + 1
  -- ny = ny' + 1
  -- nz = nz' + 1
  (lengths, cells, types) = go 0 S.empty bottomTypes S.empty S.empty
  go :: Int -> Seq Int -> Matrix Int -> Seq (Seq Int) -> Seq (Seq Int)
     -> (Seq Int, Seq (Seq Int), Seq (Seq Int))
  go k !lngths !bTypes !clls !tps
    | k == nz' = (lngths, clls, tps)
    | otherwise =
      go (k+1) (lngths |> l) tTypes (clls |> cll) (tps |> tp)
    where
    tTypes = faceType (arrayToMatrix a (k+1)) level mx
    cellTypes = elementwiseUnsafe (+) bTypes (scaleMatrix 16 tTypes)
    goodcells = whichIndicesAndItems cellTypes
    l = S.length goodcells
    cll = fmap (\(i,_) -> i + nx'*ny'*k + 1) goodcells
    tp = fmap snd goodcells
  out = M.transpose (fromLists (concatMap f [0 .. nz'-1]))
  f k = map (g k) [0 .. S.index lengths k - 1]
  g k l = [  c `mod` nx' + 1
          , (c `div` nx') `mod` ny' + 1
          , c `div` (nx'*ny') + 1
          , S.index (S.index types k) l]
      where
      c = S.index (S.index cells k) l - 1

getBasic1 :: Vector Int -> Matrix Int -> Matrix Int
getBasic1 r vivjvk = elementwiseUnsafe (+) k1 k2
  where
  nR = UV.length r
  cube1 = matrix nR 3 (\(i,j) -> getElem (r ! (i-1)) j vivjvk)
  k1 = kro1 indexArray nR
  k2 = kro2 cube1 8

-- r :: Vector Int
-- r = UV.fromList [2,2,3,4]
--
-- v :: Matrix Int
-- v = fromLists [[2,1,1],[2,1,1],[1,2,1],[2,2,1]]

getBasic2 :: Array (Int,Int,Int) Double -> Double -> Matrix Int -> Vector Double
getBasic2 a level cubeco = UV.fromList values
  where
  f i j = getElem i j cubeco - 1
  values = [a A.! (f i 1, f i 2, f i 3) - level |
            i <- [1 .. nrows cubeco - 1]] ++ [0]
