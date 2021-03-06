----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Wholemeal where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- |
--
-- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
-- True
-- >>> fun1 [1,2,3] /= fun1' [1,2,3]                                                         
-- False
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 15 /= fun2' 15
-- False

fun1' :: [Integer] -> Integer
fun1' xs = product (map (subtract 2) (filter even xs))

fun2' :: Integer -> Integer
fun2'  = sum . filter even . takeWhile (>1) . iterate (\n->if even n then n `div` 2 else 3*n + 1)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a = 
    Leaf
  | Node Int (Tree a) a (Tree a)
    deriving (Show, Eq)

mitades :: [a] -> ([a],[a])
mitades xs = splitAt (length xs `div` 2) xs 

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree xs = Node acc (foldTree ys) z (foldTree zs)
  where (ys,z:zs) = mitades xs
        acc = (length xs `div` 2) -1
   
                
----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

xorFilter :: [Bool] -> [Bool]
xorFilter = foldr (\x y -> if x then x:y else y) []
  
xor :: [Bool] -> Bool
xor xs  = odd $ length (xorFilter xs)

{-xorFilter2 :: [Bool] -> [Bool]
xorFilter2 = foldr f []
  where f x y | x = x:y
              | otherwise = y-}
-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x:y) []

-- Optional

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = undefined

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Int -> [Int]
sieveSundaram n = map (\x -> 2 * x + 1) $ removeij [1..n] 
    where
    possibleij:: Int -> (Int, [(Int, Int)])
    possibleij x = (x, [(i, j) | 
                   j <- [1..x `div` 3], 
                   i <- [1..j], 
                   i + j + 2 * i * j == x])
    removeij:: [Int] -> [Int]
    removeij = map fst 
                . filter (null . snd)
                . map possibleij                  