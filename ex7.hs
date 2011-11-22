filterAndApply f p xs = map f (filter p xs)

myAll f = foldr (\n m -> f n && m) True
myAny f = foldr (\n m -> f n || m) False

myTakeWhile _ [] = []
myTakeWhile f (x:xs) | f x 		 = x:myTakeWhile f xs
					 | otherwise = []

myDropWhile _ [] = []
myDropWhile f (x:xs) | f x 		 = myDropWhile f xs
					 | otherwise = x:xs

myFilter p = foldr (\x y -> (if p x then [x] else []) ++ y) []
myMap f = foldr (\x y -> (f x):y) []

dec2int = foldl (\n x -> n*10 + x) 0
{- [x1,x2,x3,x4] -> (((0*10 + x1)*10 + x2)*10 + x3)*10 + x4 -}
{-wow, this works well and I never thought of it this way. I had too google the answer-}

{-compose [...] exercise:-}
{-because all element of a list must have the same time.-}
{-compose is being past a list where the elements are type like so:-}
	{-[Num(a) => [a] -> a, Num(b) => [b] -> [b], Intergral(a) => [a] -> [a]]-}

{-in this list, the first element is a function that takes a list and returns a numeral.-}
{-the other two elements are functions that take lists and return lists.-}

{-so the first element is of a different type than the other two and the list is illegal.-}
