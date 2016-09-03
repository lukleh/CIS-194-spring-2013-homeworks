module W4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\a b -> if even a then (a - 2) * b else b) 1

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

-- does not look better though
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 0) . iterate nseq

nseq :: Integer -> Integer
nseq x
  | x == 1    = 0
  | even x    = x `div` 2
  | otherwise = 3 * x + 1


-- the binary tree is not ordered
data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = undefined


xor :: [Bool] -> Bool
xor = foldr (\a b -> (a || b) && not (a && b)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a b -> f a : b) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b -> f b a) base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let removeNums = [i + j + 2 * i * j | i <- [1..n], j <- [i..n], i + j + 2 * i * j <= n]
                      f nn ys
                        | elem nn removeNums = ys
                        | otherwise = nn:ys
                  in map (\x -> 2 * x + 1) $ foldr f [] [1..n]
