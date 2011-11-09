halve xs = (take n xs, drop n xs)
		   where n = length xs `div` 2

safetail [] = []
safetail xs = tail xs

safetail2 xs | null xs   = []
			 | otherwise = tail xs

safetail3 xs = if null xs then [] else tail xs

True ^ b = if b == True then True else False
False ^ _ = False

add = \x -> (\y -> x + y)
mult = \x -> (\y -> (\z -> x * z * y))
