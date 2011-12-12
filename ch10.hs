module Types where

import IO

type Pos = (Int, Int)
data Move = Mleft | Mright | Mup | Mdown

move  :: Move -> Pos -> Pos
move Mleft (x,y) = (x-1,y)
move Mright (x,y) = (x+1,y)
move Mup (x,y) = (x,y-1)
move Mdown (x,y) = (x,y+1)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = moves ms (move m p)

flip :: Move -> Move
flip Mup = Mdown
flip Mdown = Mup
flip Mleft = Mright
flip Mright = Mdown

data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect n m) = n * m

data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Types.Maybe Int
safediv _ 0 = Types.Nothing
safediv m n = Types.Just (m `div` n)

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat (n + 1) = Succ (int2nat n)

add :: Nat -> Nat -> Nat
add m n = int2nat (x + y)
          where x = nat2int m
                y = nat2int n

data Tree = Leaf Int | Node Tree Int Tree

t :: Tree
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) 
    | m == n = True
    | m < n = occurs m l
    | m > n = occurs m r

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)

vars :: Prop -> [Char]
vars (Const b) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p1 p2) = vars p1 ++ vars p2
vars (Imply p1 p2) = vars p1 ++ vars p2

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools (n + 1) = (map (False:) (bools n)) ++ (map (True:) (bools n))

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups(filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
