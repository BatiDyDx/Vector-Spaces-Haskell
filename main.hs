-- TRABAJO PRACTICO 1 - ESTRUCTURAS DE DATOS Y ALGORITMOS 2
-- JUAN BAUTISTA FIGUEREDO, BAUTISTA PEIRONE
module VecSpace where

type Axis = Int

data NdTree p = Node (NdTree p) -- subarbol izquierdo
                p -- punto
                (NdTree p) -- sub ́arbol derecho
                Axis -- eje
                | Empty
                deriving (Show)

class Punto p where
  dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
  coord :: Axis -> p -> Double -- devuelve la coordenada k-esima de un punto (comenzando de 0)
  
  dist :: p -> p -> Double -- calcula la distancia entre dos puntos
  dist x y = sum [((coord i x) - (coord i y)) ^ 2 | i <- [0..n]]
            where n = (dimension x) - 1

newtype Punto2d = P2d (Double, Double) deriving (Show, Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show, Eq)

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

-- Realiza un ordenamiento sobre una lista de tipo a,
-- comparando segun los resultados obtenidos de aplicar una funcion
-- a cada elemento
qsort :: Ord b => (a -> b) -> [a] -> [a]
qsort _ [] = []
qsort _ [x] = [x]
qsort f (x:xs) = left ++ [x] ++ right
                where
                  left = qsort f [y | y <- xs, f y <= f x]
                  right = qsort f [y | y <- xs, f y > f x]

-- Devuelve la mediana de una lista ordenada, en caso
-- de tener longitud par, devuelve el de centro derecha
median :: [a] -> Int -> a
median xs n | even n = xs !! (n `div` 2)
            | otherwise = xs !! ((n - 1) `div` 2)

nextAxis :: Axis -> Int -> Axis
nextAxis axis dimension = (axis + 1) `mod` dimension

-- Crea un arbol NdTree a partir de una lista de puntos, un eje dado
-- y la longitud de la lista
-- Se pasa la longitud de la lista para evitar calcularla en cada llamado
makeNdTree :: (Eq p, Punto p) => [p] -> Axis -> Int -> NdTree p
makeNdTree [] _  _ = Empty
makeNdTree xs axis n = Node l x r axis
                  where
                    x = median (qsort (coord axis) xs) n -- Elemento del medio, segun coordenada axis
                    naxis = nextAxis axis (dimension x) -- Eje del proximo nivel del arbol
                    (leftn, rightn) = if even n then (div n 2, (div n 2) - 1)
                                                else (div (n - 1) 2, div (n - 1) 2)
                    l = makeNdTree [y | y <- xs, coord axis y <= coord axis x, y /= x] naxis leftn -- Creacion recursiva de subarbol izq
                    r = makeNdTree [y | y <- xs, coord axis y > coord axis x] naxis rightn -- Creacion recursiva de subarbol izq

-- Crea un NdTree dada una lista de puntos
fromList :: (Eq p, Punto p) => [p] -> NdTree p
fromList xs = makeNdTree xs 0 (length xs)

-- Inserta un punto en un NdTree conservando el invariante de NdTree
insertar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
insertar x Empty = Node Empty x Empty 0
insertar x t@(Node l y r axis)
            | coord axis x <= coord axis y = if x /= y 
              then
                case l of
                  Empty -> (Node (Node Empty x Empty (nextAxis axis (dimension x))) y r axis) -- Se agrega el nodo en una hoja 
                  _     -> (Node (insertar x l) y r axis) -- Se inserta recursivamente a izquierda
              else t -- Si el punto ya esta, no se inserta
            | coord axis x > coord axis y = 
              case r of
                Empty -> (Node l y (Node Empty x Empty (nextAxis axis (dimension x))) axis) -- Se agrega el nodo en una hoja
                _     -> (Node l x (insertar x r) axis) -- Se inserta recursivamente a derecha

-- Consigue el punto con menor coordenada segun el eje especificado
minimumNd :: Punto p => NdTree p -> Axis -> p
minimumNd (Node Empty y Empty axis) _ = y
minimumNd (Node l y r axis) eje | axis == eje = minimumNd l eje -- Sabemos que el minimo se encuentra a izquierda
                                | otherwise = let (x,y) = (minimumNd l eje, minimumNd r eje)
                                              in if (coord eje x) <= (coord eje y) then x else y -- El minimo puede estar en cualquier subarbol

-- Consigue el punto con mayor coordenada segun el eje especificado
maximumNd :: Punto p => NdTree p -> Axis -> p
maximumNd (Node Empty y Empty axis) _ = y
maximumNd (Node l y r axis) eje | axis == eje = maximumNd r eje -- Sabemos que el minimo se encuentra a derecha
                                | otherwise = let (x,y) = (maximumNd l eje, maximumNd r eje)
                                              in if (coord eje x) >= (coord eje y) then x else y -- El maximo puede estar en cualquier subarbol

-- Elimina un punto del arbol conservando el invariante de NdTree
-- Prioriza la eliminacion recursiva a derecha si es posible
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty
eliminar x t@(Node Empty y Empty axis) | x == y = Empty
                                       | otherwise = t
eliminar x t@(Node l y r axis)
        | x /= y = if (coord axis x) <= (coord axis y) then (Node (eliminar x l) y r axis)
                   else (Node l y (eliminar x r) axis) -- Se elimina recursivamente a izq o derecha segun corresponde
        | otherwise = case r of -- Si se encuentra el nodo
                        Empty -> let max = maximumNd l axis -- Caso en que no hay derecho, se reemplaza por el nodo con
                                     l' = eliminar max l    -- el maximo a izquierda, y este se elimina recursivamente
                                 in (Node l' max r axis)
                        _ ->     let min = minimumNd r axis -- Caso en que hay derecho, se reemplaza por el nodo con
                                     r' = eliminar min r    -- el minimo a derecha, y este se elimina recursivamente
                                 in (Node l min r' axis)


-- IMPLEMENTACION DE EJERCICIO 5 PARA N DIMENSIONES

{-
- El primer punto contiene las coordenadas minimas de cada eje que
- establecen los limites del prisma en n-dimensiones, mientras que el
- segundo punto es analogo pero con la maximas coordenadas respectivamente
-}

type NPrism p = (p, p) -- declaracion de tipo para prima de n dimensiones

-- Determina si un punto n-dimesional se encuentra en un prima n-dimensional
inNRegion :: Punto p => p -> NPrism p -> Bool
inNRegion p (minP, maxP) = and [let x = (coord i p) in (coord i minP) <= x && x <= (coord i maxP) | i <- [0..n]]
                            where
                              n = (dimension p) - 1

-- Determina si la coordenada i-esima de un punto dado es menor al minimo limite i-esimo del prisma
discardIfLower :: Punto p => p -> NPrism p -> Axis -> Bool
discardIfLower p (minX, _) axis = (coord axis p) < (coord axis minX)

-- Determina si la coordenada i-esima de un punto dado es mayor al maximo limite i-esimo del prisma
discardIfGreater :: Punto p => p -> NPrism p -> Axis -> Bool
discardIfGreater p (_, maxX) axis = (coord axis p) > (coord axis maxX)

-- Determina cuales puntos de un arbol NdTree pertencen a un prisma n-dimensional
ortogonalNSearch :: Punto p => NdTree p -> NPrism p -> [p]
ortogonalNSearch Empty _ = []
ortogonalNSearch (Node l p r axis) prism =
        let ps = if inNRegion p prism then [p] else [] -- Si el punto se encuentra en el prisma lo agrega, sino no
            ls = if (discardIfLower p prism axis) 
                 then [] -- si es menor, descarta el subarbol izquierdo
                 else ortogonalNSearch l prism -- sino, busca recursivamente en el subarbol izquierdo
            rs = if (discardIfGreater p prism axis) 
                 then [] -- si es mayor, descarta el subarbol derecho
                 else ortogonalNSearch r prism -- sino, busca recursivamente en el subarbol derecho
        in ps ++ rs ++ ls

type Rect = (Punto2d, Punto2d) -- Esquina inferior izquierda y esquina superior derecha

-- Determina si un punto de dos dimensiones se encuentra dentro de un rectangulo
inRegion :: Punto2d -> Rect -> Bool
inRegion p rect = inNRegion p rect

-- Determina cuales puntos de un arbol NdTree pertenecen a un rectangulo dado
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch t rect = ortogonalNSearch t rect
