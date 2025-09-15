import Debug.Trace

-- total [] = 0
-- total (x:xs) = x + total xs

-- count [] = 0
-- count (x:xs) = 1 + count xs

-- average xs = (sum xs) / (count xs)

averageLet xs =
  let total = sum xs
      count = fromIntegral $ length xs
   in total / count

averageWhere xs = total / count
  where
    total = sum xs
    count = fromIntegral $ length xs

total1 [] = 0
total1 (x : xs) = x + total1 xs

total2 xs =
  if null xs
    then 0
    else (head xs) + total2 (tail xs)

total3 xs
  | null xs = 0
  | otherwise = (head xs) + total3 (tail xs)

total4 xs | trace ("input:" ++ show xs) False = undefined
total4 xs =
  case xs of
    [] -> 0
    (y : ys) -> y + total4 ys


odd' x | trace "odd called" False = undefined
odd' x = x `mod` 2 /= 0
nums = [1..100]
getNOdds n = take n $ filter odd' nums

newtype USD = USD Double deriving Show
newtype INR = INR Double deriving Show

toINR (USD x) = INR $ x * 89
toUSD (INR x) = USD $ x / 89

data Shape = Square Double | Rectangle Double Double
    deriving Show

area x = case x of
    (Square s) -> s * s
    (Rectangle l h) -> l * h

area1 (Square s) = s * s
area1 (Rectangle l h) = l * h

data BinaryIntTree = IEmpty | INode Int BinaryIntTree BinaryIntTree
    deriving Show

data BinaryStringTree = SEmpty | SNode String BinaryStringTree BinaryStringTree
    deriving Show


data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a)