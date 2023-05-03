module VecSpace where

type Axis = Int

data NdTree p = Node (NdTree p) -- subarbol izquierdo
                p -- punto
                (NdTree p) -- sub ́arbol derecho
                Axis -- eje
                | Empty
                deriving (Show)

class Eq p => Punto p where
  dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
  coord :: Int -> p -> Double -- devuelve la coordenada k-esima de un punto (comenzando de 0)
  
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist x y = sum [((coord i x) - (coord i y)) ^ 2 | i <- [0..n]]
            where n = (dimension x) - 1

newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show, Eq)

-- instance Punto a => Eq a where
--   p == q = (dist p q) == 0

instance Punto Punto2d where
  dimension _ = 2

  coord 0 (P2d (x,_)) = x
  coord 1 (P2d (_,y)) = y
  coord _ _ = error "Coordenada invalida"

instance Punto Punto3d where
  dimension _ = 3

  coord 0 (P3d (x,_,_)) = x
  coord 1 (P3d (_,y,_)) = y
  coord 2 (P3d (_,_,z)) = z
  coord _ _ = error "Coordenada invalida"


qsort :: Ord b => (a -> b) -> [a] -> [a]
qsort _ [] = []
qsort _ [x] = [x]
qsort f (x:xs) = left ++ [x] ++ right
                where
                  left = qsort f [y | y <- xs, f y <= f x]
                  right = qsort f [y | y <- xs, f y > f x]

median :: [a] -> Int -> a
median xs n | even n = xs !! (n `div` 2)
            | otherwise = xs !! ((n - 1) `div` 2)

nextAxis :: Axis -> Int -> Axis
nextAxis axis dimension = (axis + 1) `mod` dimension

makeNdTree :: Punto p => [p] -> Axis -> Int -> NdTree p
makeNdTree [] _  _ = Empty
makeNdTree xs axis n = Node l x r axis
                  where
                    x = median (qsort (coord axis) xs) n
                    naxis = nextAxis axis (dimension x)
                    (leftn, rightn) = if even n then (div n 2, (div n 2) - 1)
                                                else (div (n - 1) 2, div (n - 1) 2)
                    l = makeNdTree [y | y <- xs, coord axis y <= coord axis x, y /= x] naxis leftn
                    r = makeNdTree [y | y <- xs, coord axis y > coord axis x] naxis rightn

fromList :: Punto p => [p] -> NdTree p
fromList xs = makeNdTree xs 0 (length xs)

insertar :: Punto p => p -> NdTree p -> NdTree p
insertar x Empty = Node Empty x Empty 0
insertar x t@(Node l y r axis)
            | coord axis x <= coord axis y = 
              if x /= y 
                then
                  case l of
                    Empty -> (Node (Node Empty x Empty (nextAxis axis (dimension x))) y r axis) 
                    _     -> (Node (insertar x l) y r axis)
                else t
            | coord axis x > coord axis y = 
              case r of
                Empty -> (Node l y (Node Empty x Empty (nextAxis axis (dimension x))) axis) 
                _     -> (Node l x (insertar x r) axis)

minimumNd :: Punto p => NdTree p -> Axis -> p
minimumNd (Node Empty y Empty axis) _ = y
minimumNd (Node l y r axis) eje | axis == eje = minimumNd l eje
                                | otherwise = let (x,y) = (minimumNd l eje, minimumNd r eje)
                                              in if (coord eje x) <= (coord eje y) then x else y

maximumNd :: Punto p => NdTree p -> Axis -> p
maximumNd (Node Empty y Empty axis) _ = y
maximumNd (Node l y r axis) eje | axis == eje = maximumNd r eje
                                | otherwise = let (x,y) = (maximumNd l eje, maximumNd r eje)
                                              in if (coord eje x) >= (coord eje y) then x else y

eliminar :: Punto p => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty
eliminar x t@(Node Empty y Empty axis) | x == y = Empty
                                       | otherwise = t
eliminar x t@(Node l y r axis)
        | x /= y = if (coord axis x) <= (coord axis y) then (Node (eliminar x l) y r axis)
                   else (Node l y (eliminar x r) axis) 
        | otherwise = case r of
                        Empty -> let max = maximumNd l axis
                                     l' = eliminar max l
                                 in (Node l' max r axis)
                        _ ->     let min = minimumNd r axis
                                     r' = eliminar min r
                                 in (Node l min r' axis)
              
type Rect = (Punto2d, Punto2d) -- Esquina inferior izquierda y esquina superior derecha

inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (x,y)) ((P2d (x0, y0)), (P2d (x1, y1))) = x0 <= x && x <= x1
                                      && y0 <= y && y <= y1

fueraIzq :: Punto2d -> Rect -> Bool
fueraIzq (P2d (x,_)) ((P2d (x0,_)), _) = x0 > x

fueraDer :: Punto2d -> Rect -> Bool
fueraDer (P2d (x,_)) (_, (P2d (x1,_))) = x1 < x

fueraArr :: Punto2d -> Rect -> Bool
fueraArr (P2d (_,y)) (_, (P2d (_, y1))) = y1 < y

fueraAbj :: Punto2d -> Rect -> Bool
fueraAbj (P2d (_,y)) ((P2d (_,y0)), _) = y0 > y

branchLeft :: Punto2d -> Rect -> Axis -> Bool
branchLeft p rect 0 = not (fueraIzq p rect)
branchLeft p rect 1 = not (fueraAbj p rect)

branchRight :: Punto2d -> Rect -> Axis -> Bool
branchRight p rect 0 = not (fueraDer p rect)
branchRight p rect 1 = not (fueraArr p rect)

ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node l p r axis) rect =
        let ps = if inRegion p rect then [p] else []
            ls = if (branchLeft p rect axis) --- NOMBRE AUXILIAR 
                 then ortogonalSearch l rect
                 else []
            rs = if (branchRight p rect axis) --- NOMBRE AUXILIAR 
                 then ortogonalSearch r rect
                 else []
        in ps ++ rs ++ ls
