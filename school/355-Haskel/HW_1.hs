--levi chapman
--11761930
--CUSTOM LIST OPPERATIONS 


-- droplist
mydroplist _ [] = []
mydroplist 1 (y:ys) = [ys]
mydroplist x (y:ys) = mydroplist (x-1) ys

isEven 0 = True
isEven 1 = False
isEven n = isEven (n `mod` 2)

mysplitByCondition _ []= ([],[])
mysplitByCondition fn (x:xs) 
    | fn x = (x:ys, zs)
    | otherwise = (ys, x:zs)
    where
        (ys,zs) = mysplitByCondition fn xs

 




--zipList
myziplists _ [] = []
myziplists [] _ = []
myziplists (x:xs) (y:ys) = (x,y) : myziplists xs ys


--interleaveLists





