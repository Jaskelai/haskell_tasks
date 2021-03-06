module Part3 where

import Data.List (genericLength)
import Data.Map (fromListWith, toList)

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 n
  | n == 1 = False
  | n == 2 = True
  | otherwise = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) (2 : filter prob18 [3, 5 ..]))

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 x = map (\d -> (d, factorize d x)) (primeDivisors x)

primeDivisors :: Integer -> [Integer]
primeDivisors x = filter prob18 (getDivisors x)

factorize :: Integer -> Integer -> Int
factorize divisor number
  | mod number divisor == 0 = 1 + factorize divisor (div number divisor)
  | otherwise = 0

roundedCount :: Integral a => a -> a
roundedCount x = round (sqrt (fromIntegral x))

getDivisors :: Integer -> [Integer]
getDivisors n = halfDivisors ++ getAllDivisors n halfDivisors []
  where
    halfDivisors = filter isDivisor [1 .. (roundedCount n)]
    isDivisor candidate = mod n candidate == 0

getAllDivisors :: Integer -> [Integer] -> [Integer] -> [Integer]
getAllDivisors _ [] acc = acc
getAllDivisors n (x : xs) acc =
  let a = div n x
   in if a == x
        then getAllDivisors n xs acc
        else getAllDivisors n xs (a : acc)

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 x = x == sum (init (getDivisors x))

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 = getDivisors

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 str = product (map mapper (words str))
  where
    mapper _str = genericLength (filter (== 'i') _str)

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 number = checkSum 1 0
  where
    checkSum _num _sum
      | _sum == number = True
      | _sum > number = False
      | otherwise = checkSum (_num + 1) (_sum + _num)

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 num = reversal (abs num) == abs num

reversal :: Integral a => a -> a
reversal = go 0
  where
    go a 0 = a
    go a b = let (q, r) = b `quotRem` 10 in go (a * 10 + r) q

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 x y = sum (_getDivisors x) == x + y && sum (_getDivisors y) == x + y

_getAllDivisors :: Integer -> [Integer] -> [Integer] -> [Integer]
_getAllDivisors _ [] acc = acc
_getAllDivisors n (x:xs) acc =
  let a = (n `div` x)
  in if a == x
    then _getAllDivisors n xs acc
    else _getAllDivisors n xs (a : acc)

_getDivisors :: Integer -> [Integer]
_getDivisors n = halfDivisors ++ _getAllDivisors n halfDivisors []
  where
    halfDivisors = filter isDivisor [1..(roundedCount n)]
    isDivisor candidate = n `mod` candidate == 0

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 _sum (x:xs) = case findComplement _sum x xs of
    Nothing -> prob27 _sum xs
    (Just compl) -> Just (x, compl)
  where
    findComplement _ _ [] = Nothing
    findComplement _sum item (_x:_xs)
      | item + _x == _sum = Just _x
      | otherwise = findComplement _sum item _xs

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609
prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- range, y <- range])))
            where
               range = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 k = head (filter (\t -> length (getDivisors t) >= k) getTriangleNums)

getTriangleNums :: [Integer]
getTriangleNums = map (\n -> n * (n + 1) `div` 2) [0..]

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
