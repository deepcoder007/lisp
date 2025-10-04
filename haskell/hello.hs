
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

descList :: [a] -> String
descList xs =  case xs of
              []  -> "The list is empty"
              [x] -> "The list has one element"
              x:x2:x3 -> "The list has atleast 2 element"

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStrLn "The input line is "
            putStrLn xs
            putStrLn "The input line length is "
            putStrLn ( show (length xs))

type Pos = (Int, Int)
goto :: Pos -> IO ()
goto (x,y) =  putStr("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")

printList :: [Int] -> [IO ()] 
printList xs = [putStrLn (show x) | x <- xs]

tList :: [Int] -> [String] 
tList xs = [show (x + 10) | x <- xs]

-- simple custom type example
data Move = Left | Right | Up | Down

printMove :: Move -> Move
printMove x = x 

-- Recursive type example
data Nat = Zero | Succ Nat

depth :: Nat -> Integer
depth Zero = 0
depth (Succ x) = 1 + depth x

data CList a = Nil | Cons a (CList a) deriving (Show)

len :: Num a => CList a -> Integer
len Nil = 0
len (Cons _ xs) = 1 + len xs

putCList ::Num a => CList a -> IO ()
putCList Nil = putStrLn "End"
putCList (Cons x xs) = do 
                         putStrLn "Element"
                         putCList xs

flatten :: Num a => CList a -> [a]
flatten Nil = []
flatten (Cons x xs) = x:(flatten xs)

main :: IO ()
main = putStrLn "Hello, World"


