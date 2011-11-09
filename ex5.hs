intSqr = sum[x^2 | x <- [1..100]]

repli n a = [a | _ <- [1..n]]

pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors n = [x | x <- [1..n], y <- [1..n], x * y == n]
perfects n = [x | x <- [1..n], sum(drop 1 (reverse(factors x))) == x]

find k t = [v | (k',v) <- t, k == k']
positions x xs = find x (zip xs [0..n])
				 where n = length xs - 1

scalarproduct xs ys = sum[x*y | (x,y) <- zip xs ys]
