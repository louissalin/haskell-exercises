module Calculator where

import Char
import Monad
import IO

{-The monad of parsers-}
{----------------------}

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

{-Basic parsers-}
{---------------}

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

{-Choice-}
{--------}

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

{-Derived primitives-}
{--------------------}

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  +++ nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

{-Ignoring spacing-}
{------------------}

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)

-- Evaluator

expr :: Parser Int
expr = do t <- term
          (do symbol "+"
              e <- expr
              return (t + e)
           +++ (do symbol "-"
                   e <- expr
                   return (t - e))
           +++ return t)

term :: Parser Int
term = do f <- expTerm
          (do symbol "*"
              t <- term
              return (f * t)
           +++ (do symbol "/"
                   t <- term
                   return (f `div` t))
           +++ return f)

expTerm :: Parser Int
expTerm = do f <- factor
             (do symbol "^"
                 f2 <- expTerm
                 return (f ^ f2)
              +++ return f)

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         +++ natural

----------------
-- calculator --
----------------

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

box :: [String]
box = ["+---+---+---+---+",
	   "|               |",
	   "+---+---+---+---+",
	   "| q | c | d | = |",
	   "+---+---+---+---+",
	   "| 1 | 2 | 3 | + |",
	   "+---+---+---+---+",
	   "| 4 | 5 | 6 | - |",
	   "+---+---+---+---+",
	   "| 7 | 8 | 9 | * |",
	   "+---+---+---+---+",
	   "| 0 | ( | ) | / |",
	   "+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
          where 
            standard = "qcd=1234567890()+-*/"
            extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat(1,y) xs | (y, xs) <- zip[1..13] box]

display :: String -> IO ()
display xs = do writeat (3,2) "             "
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if elem c buttons
                then process c xs
                else calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
             [(n, rest)] -> if (length rest) == 0 
                                then do clearError
                                        calc (show n)
                                else do writeat (25, 1) rest
                                        calc xs
             _ -> calc xs

clearError :: IO ()
clearError = writeat (25, 1) "            "

clear :: IO ()
clear = do clearError
           calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

------------------
-- game of life --
------------------

width :: Int
width = 5

height :: Int
height = 5

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

emptyBoard :: Board
emptyBoard = []

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),
                          (x,y-1),
                          (x+1,y-1),
                          (x-1,y),
                          (x+1,y),
                          (x-1,y+1),
                          (x,y+1),
                          (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                     isEmpty b p,
                     liveneighbs b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x:rmdups(filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

removeOld :: [Pos] -> IO ()
removeOld [] = return ()
removeOld (p:ps) = do writeat p " "
                      removeOld ps

life :: Board -> Board -> IO ()
life prevBoard b = do removeOld [p | p <- prevBoard, 
                                     not (elem p b)]
                      showcells b
                      wait 50000
                      life b (nextgen b)
                      quit

runLife :: Board -> IO ()
runLife b = do cls
               life emptyBoard b

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]


myGetLine :: IO String
myGetLine = do x <- getChar
               if x == '\n'
                  then return []
                  else do xs <- getLine
                          return (x:xs)

readLine = get ""
get xs = do x <- getChar
            case x of
                '\n' -> return xs
                '\DEL' -> if null xs
                    then get xs
                    else do putStr "\ESC[1D \ESC[1D"
                            get (init xs)
                _ -> get (xs ++ [x])
