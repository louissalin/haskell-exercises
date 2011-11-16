myExp x 1 = x
myExp x (n + 1) = x * myExp x n

myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs

myRep 0 _ = []
myRep (n + 1) x = x:myRep n x

myElem y [] = False
myElem y (x:xs) = x == y || myElem y xs

myIndex (x:xs) 0 = x
myIndex (x:xs) (n + 1) = myIndex xs n

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge (x:xs) ys = myMerge xs (smaller ++ [x] ++ larger)
	where
		smaller = [y | y <- ys, y <= x]
		larger  = [y | y <- ys, y > x]

half xs = (take n xs, drop n xs)
	where
		n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = myMerge (msort firstHalf) (msort sndHalf)
	where
		firstHalf = fst(half xs)
		sndHalf = snd(half xs)

mySum [] = 0
mySum (x:xs) = x + mySum(xs)

myTake _ [] = []
myTake 0 _ = []
myTake (n + 1) (x:xs) | n >= length xs 	= x:xs
					  | otherwise       = x:(myTake n xs)

myLast :: [a] -> a
myLast (x:xs) | null xs = x
			  | otherwise = myLast xs
