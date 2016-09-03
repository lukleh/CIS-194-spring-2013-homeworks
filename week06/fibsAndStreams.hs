
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fibs1' 0
  where fibs1' n = fibs1' n : fibs1' (n + 1)

fibs2 :: [Integer]
fibs2 = fibs2' 0 1
  where fibs2' a b = a : fibs2' b (a + b)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

instance Show a => Show (Stream a) where
  show a = showN a 20
    where showN (Cons a b) n
            | n < 1 = ""
            | otherwise = show a ++ "|" ++ showN b (n - 1)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+1) 0
