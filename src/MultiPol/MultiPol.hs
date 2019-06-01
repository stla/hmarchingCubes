-- {-# LANGUAGE ExistentialQuantification #-}
module MultiPol.MultiPol
  (evalPoly, derivPoly, fromListOfMonomials, Polynomial(..), Monomial(..))
  where

data Polynomial a = M (Monomial a)
                | Polynomial a :+: Polynomial a
                | Polynomial a :*: Polynomial a
                deriving(Show, Eq)

data Monomial a = Monomial {
        coefficient :: a,
        powers      :: (Int,Int,Int)
    }
    deriving(Show, Eq)

zero :: RealFloat a => Polynomial a
zero = M Monomial { coefficient = 0, powers = (0,0,0) }

derivMonomial :: RealFloat a => Monomial a -> Char -> Polynomial a
derivMonomial mono var = let (px,py,pz) = powers mono in
  case var of
    'x' -> if px >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral px
                     , powers = (px-1,py,pz)
          }
      else zero
    'y' -> if py >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral py
                     , powers = (px,py-1,pz)
          }
      else zero
    'z' -> if pz >= 1
      then M Monomial {coefficient = coefficient mono * fromIntegral pz
                     , powers = (px,py,pz-1)
          }
      else zero
    _ -> error "only variables x,y,z are allowed"

derivPoly :: RealFloat a => Polynomial a -> Char -> Polynomial a
derivPoly pol var = case pol of
  M mono -> derivMonomial mono var
  a :+: b -> derivPoly a var :+: derivPoly b var
  a :*: b -> (derivPoly a var :*: b) :+: (a :*: derivPoly b var)

evalMonomial :: RealFloat a => (a, a, a) -> Monomial a -> a
evalMonomial (x,y,z) monomial =
  coefficient monomial * x^px * y^py * z^pz
  where
    (px,py,pz) = powers monomial

evalPoly :: RealFloat a => Polynomial a -> (a,a,a) -> a
evalPoly pol xyz = case pol of
  M mono -> evalMonomial xyz mono
  a :+: b -> evalPoly a xyz + evalPoly b xyz
  a :*: b -> evalPoly a xyz * evalPoly b xyz

fromListOfMonomials :: RealFloat a => [Monomial a] -> Polynomial a
fromListOfMonomials ms = foldl1 (:+:) (map M ms)
