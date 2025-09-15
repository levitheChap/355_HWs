mymap f = foldr (\x acc -> f x : acc) []

-- mymap (\x -> x * 2) [1, 2, 3, 4]

-- foldr (\x acc -> (\x -> x * 2) x : acc ) [] [1, 2, 3, 4]



-- Custom list operations

-- length
len :: Num a1 => [a2] -> a1
len []     = 0
len (_:xs) = 1 + len xs

-- applyDouble
applyDouble :: t -> (t -> t) -> t
applyDouble x fn = fn (fn x)

-- mymap
mymap :: (t -> a) -> [t] -> [a]
mymap _ []     = []
mymap fn (x:xs) = fn x : mymap fn xs

-- elem1
elem1 :: Eq t => t -> [t] -> Bool
elem1 _ [] = False
elem1 x (y:ys)
  | x == y    = True
  | otherwise = elem1 x ys