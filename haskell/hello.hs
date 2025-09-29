
module Main where

custom_add x = x+x

quad x =  (custom_add (custom_add x))

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                  smaller = [a | a <- xs, a <= x]
                  larger = [a | a <- xs, a >x]


csignum :: Int -> Int
csignum n 
      | n < 0     = -1
      | n == 0    = 0
      | otherwise = 1

cons :: a -> (b -> a)
cons x = \_ -> x

factors :: Int -> [Int]
factors n = [x| x <- [1..n], n `mod` x == 0]

factorial1 :: Int -> Int
factorial1 n = product [1..n]

factorial2 :: Integer -> Integer
factorial2 0 = 1
factorial2 n  = n * (factorial2 (n - 1))

insert :: Ord a => a -> [a] -> [a]
insert x []  =   [x]
insert x (y:ys)   |   x <= y        = x:y:ys
                  |  otherwise      = y:insert x ys

main :: IO ()
main = putStrLn "Hello, World"


