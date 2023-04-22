module VecSpace where

data NdTree p = Node (NdTree p) -- subarbol izquierdo
                p -- punto
                (NdTree p) -- sub ́arbol derecho
                Int -- eje
                | Empty
                deriving (Eq, Ord, Show)

class Punto p where
  dimension :: p -> Int -- devuelve el n ́umero de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-esima de un punto (comenzando de 0)
  
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist x y = sum [((coord i x) - (coord i y)) ^ 2 | i <- [0..n]]
            where n = (dimension x) - 1

newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show, Eq)

instance Punto Punto2d where
  dimension :: Punto2d -> Int
  dimension _ = 2

  coord :: Int -> Punto2d -> Double
  coord 0 (P2d (x,_)) = x
  coord 1 (P2d (_,y)) = y
  coord _ _ = error "Coordenada invalida"

instance Punto Punto3d where
  dimension :: Punto3d -> Int
  dimension _ = 3

  coord :: Int -> Punto3d -> Double
  coord 0 (P3d (x,_,_)) = x
  coord 1 (P3d (_,y,_)) = y
  coord 2 (P3d (_,_,z)) = z
  coord _ _ = error "Coordenada invalida"

type Axis = Int

qsort :: Ord b => (a -> b) -> [a] -> [a]
qsort _ [] = []
qsort _ [x] = [x]
qsort f (x:xs) = left ++ [x] ++ right
                where
                  left = qsort f [y | y <- xs, f y <= f x] --
                  right = qsort f [y | y <- xs, f y > f x]

median :: [a] -> Int -> a
median xs n | even n = xs !! (n `div` 2)
            | otherwise = xs !! ((n - 1) `div` 2)

nextAxis :: Axis -> Int -> Axis
nextAxis axis dimension = (axis + 1) `mod` dimension

makeNdTree :: (Eq p, Punto p) => [p] -> Axis -> Int -> NdTree p
makeNdTree [] _  _ = Empty
makeNdTree xs axis n = Node l x r axis
                  where
                    x = median (qsort (coord axis) xs) n
                    naxis = nextAxis axis (dimension x)
                    (leftn, rightn) = if even n then (div n 2, (div n 2) - 1)
                                                else (div (n - 1) 2, div (n - 1) 2)
                    l = makeNdTree [y | y <- xs, coord axis y <= coord axis x, y /= x] naxis leftn
                    r = makeNdTree [y | y <- xs, coord axis y > coord axis x] naxis rightn

fromList :: (Eq p, Punto p) => [p] -> NdTree p
fromList xs = makeNdTree xs 0 (length xs)

insertar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
insertar x Empty = Node Empty x Empty 0
insertar x t@(Node l y r axis)| coord axis x <= coord axis y = 
                                  if x /= y 
                                    then
                                      case l of
                                        Empty -> (Node (Node Empty x Empty (nextAxis axis (dimension x))) y r axis) 
                                        _     ->  (Node (insertar x l) y r axis)
                                    else t
                                | xi > yi = 
                                  case r of
                                    Empty -> (Node l y (Node Empty x Empty (nextAxis axis (dimension x))) axis) 
                                    _     -> (Node l x (insertar x r) axis) 
                                where 
                                  xi = coord axis x
                                  yi = coord axis y
