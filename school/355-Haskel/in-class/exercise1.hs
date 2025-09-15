-- sum of n
sumOfN :: (Eq t, Num t) => t -> t
sumOfN 0 = 0
sumOfN n = n + sumOfN (n - 1)

-- add
add :: Num a => a -> a -> a
add x y = x + y

-- factorial
fac :: (Eq t, Num t) => t -> t
fac 0 = 1
fac n = n * fac (n - 1)

-- fibonacci
fib :: (Eq t, Num t, Num a) => t -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- isEven
isEven :: (Eq t, Num t) => t -> Bool
isEven 0 = True
isEven 1 = False
isEven n = isEven (n - 2)

-- sum of list
sumLs :: Num a => [a] -> a
sumLs [] = 0
sumLs (x:xs) = x + sumLs xs

-- main program
main :: IO ()
main = do
  putStrLn "Enter number: "
  str <- getLine
  let num = read str :: Int
  putStrLn $ "Answer: " ++ show (sumOfN num)