
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

main :: IO ()
main = putStrLn "Hello, World"

