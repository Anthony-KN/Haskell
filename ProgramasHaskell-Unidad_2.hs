
--1: Media de tres numeros
mediaNumeros x y z = (x + y + z) / 3

--2: Volumen de una esfera
volumenEsfera x = (4/3 * pi) * (x ^ 3)

--3: 10 numeros impares inciando con en 13
imparNumero = [13,15..32]

--4: obtener el maximo de 3 numeros
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximo de una lista vacia"
maximum' [x] = x
maximum' (x:xs) = x `max` (maximum' xs)

--5: Rotar una lista
rotar :: [a] -> [a] 
rotar [] = []
rotar xs = zipWith const (drop 1 (cycle xs)) xs

--6: Realiza un programa que permita generar un intervalo 
--de la suma de los cubos de los primeros n números.
-- Ejemplo suma = 1 + 8 + 27 + n 

intervaloSuma:: Int -> Int 
intervaloSuma n = sum [x^3 | x <- [1..n]]

--7: Realiza un programa que permita generar un 
--intervalo de los cuadrados de n números mayores a 100.

intervaloSumaMayor n = [x^2 | x <- [1..n], x^2 > 100]

--8: Realiza un programa que permita generar un intervalo 
--de n numeros entre 20 y 60
intervaloN n =  [ x | x <- [20..n], x >= 20, x <= 60]

--9: Realiza un programa que solicite dos argumentos de 
--tipo Double para calcular la hipotenusa de un 
--triángulo rectángulo y retorne un valor de tipo Double.
hipoTenusa:: Double -> Double -> Double
hipoTenusa x y = sqrt(x^2 + y^2)

--10: Crear un programa que por medio de recursión calcule 
--la suma de los cuadrados de n números.
sumaNum::Int->Int
sumaNum 0=0
sumaNum n = n^2 + sumaNum(n-1)
			