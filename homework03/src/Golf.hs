----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Golf where
import Data.List

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

skips :: [a] -> [[a]]
skips xs = [every n xs | n <- [1..length xs]]
  where every n xs' = case drop (n-1) xs' of
                      []     -> []
                      (y:ys) -> y : every n ys

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise = localMaxima (y:z:zs)
localMaxima _ = []  

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

histogram :: [Integer] -> String
histogram xs = unlines . transpose . fmap (reverse . insert) . freqs $ xs
  where freqs ns = fmap (\n->(n, length . filter (==n) $ ns)) [0..9] 
        insert (n, f) = take (2 + length xs) $ show n ++ "=" ++ replicate f '*' ++ repeat ' '

