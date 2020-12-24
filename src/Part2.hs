module Part2 where

import Part2.Types
import Data.Maybe (isJust, fromJust)

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 x = case x of
  RED -> 'R'
  GREEN -> 'G'
  BLUE -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
getInt :: ColorPart -> Int
getInt x = case x of
    Red int -> int
    Green int -> int
    Blue int -> int

prob7 :: ColorPart -> Bool
prob7 x = getInt x >= 0 && getInt x <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 x y = case y of
    Red int -> x { red = red x + int }
    Green int -> x { green = green x + int }
    Blue int -> x { blue = blue x + int }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 = getInt

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color r g b)
    | r > g && r > b = Just (Red r)
    | g > r && g > b = Just (Green g)
    | b > g && b > r = Just (Blue b)
    | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 (Tree _left _root _right) = _root + 
                                  (if isJust _left then prob11 $ fromJust _left else 0) +
                                   (if isJust _right then prob11 $ fromJust _right else 0)

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 = checkTree

checkTree :: Ord a => Tree a -> Bool
checkTree tree = checkSide (left tree) (root tree) False && checkSide (right tree) (root tree) True

checkSide :: Ord a => Maybe (Tree a) -> a -> Bool -> Bool
checkSide Nothing _ _ = True
checkSide (Just tree) parent isRight = case isRight of 
    True -> root tree >= parent && checkTree tree
    False -> root tree < parent && checkTree tree

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 x tree
  | root tree == x = Just tree
  | root tree > x = maybe Nothing (prob13 x) (left tree)
  | root tree < x = if isJust (right tree) then prob13 x $ fromJust (right tree) else Nothing
  | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = if isJust (right tree) then rotate $ fromJust (right tree) else tree
    where
        rotate _tree = _tree { left = Just tree { right = left _tree } }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = if isJust (left tree) then rotate $ fromJust (left tree) else tree
    where
        rotate _tree = _tree { right = Just tree { left = right _tree } }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
