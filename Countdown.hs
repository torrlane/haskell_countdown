module Main(main) where
{-
Take a list of numbers from the command line - the first one being the target.
The others should be rearranged and used with the simple arithmetic
operators in such a way that the result is equal to the target number. This
algorithm only returns solutions that use all of the available numbers

Compile:
> ghc -o Countdown Countdown.hs

Run:
> ./Countdown 952 25 50 75 100 3 6
(((((100+6)*3)*75)-50)/25)
(((((100+3)*6)*75)/50)+25)
(((((100+6)*75)*3)-50)/25)
((((75*6)*(100+3))/50)+25)
(((100+3)*((75*6)/50))+25)
(((((100+3)*75)*6)/50)+25)
((((75*3)*(100+6))-50)/25)
"Found 7 solutions"
-}
import System.Environment
import Data.List
main = do
        args <- getArgs
        let target = asInt $ head args 
        let values = asInts $ tail args
	print $ "target: " ++ show target
	print $ "values: " ++ show values
	let exprs = makeExprs $ permutations values
	let solutions = findSolutions exprs target
	
	-- print each solution on a newline
	mapM_ print solutions
	let num_solutions = length solutions
	print $ "Found " ++ show num_solutions  ++ " solutions"

{-
Returns all the pivots of the list except those that contain an empty list.
i.e. pivot [1,2,3] = [ ([1],[2,3]), ([1,2],[3]) ]
 -}
pivot :: [Int] -> [([Int], [Int])]
pivot xs =  init $ tail $ zip (inits xs) (tails xs)

{-
Takes a List of Expressions and a target number and returns all the Expressions
that evaluate to the target number
-}
findSolutions :: [Expr] -> Int -> [Expr]
findSolutions es t = filter (isEqual t) es
--[ e | e <- es, eval e == t ] 

isEqual :: Int -> Expr -> Bool
isEqual t e = eval e == t

{-
This is the nuts and bults of the algorithm.

Takes a list of Integers and generates all the possible Expr's that
can be generated from them without changing the order of the Integers
(although bracketing can change the order of evaluation)

It does this through "pivoting" around each point in the list. The "pivot"
separates the list into two non-empty lists. For each Expression generated 
from the left and right list, a set of new Expressions will be created, by 
combining them with each of the 4 operators.

The call to "valid" avoids divide-by-zero errors later on in the algorithm,
and also reduces the search space by deduping equivalent expressions 
(i.e. a+b, b+a)
 -}
makeExpr :: [Int] -> [Expr]
makeExpr (x:[]) = [Num x]
makeExpr xs = [BinaryExpr (lexpr) op (rexpr) | 
			(left, right) <- pivot xs, 
			lexpr <- makeExpr left, 
			rexpr <- makeExpr right, 
			op <- ops,
			valid (eval lexpr) op (eval rexpr),
			dedupe (eval lexpr) op (eval rexpr) ]

{-
Takes a List of List of Integer, and returns all the Expressions that can
be made from each of the Lists, without changing the order of the elements.
-}
makeExprs :: [[Int]] -> [Expr]
makeExprs xss = [ e | xs <- xss, e <- makeExpr xs]

{- 
The Expr datatype represents a mathematical Expression.
This can be either a simple number or a Binary operation on two Expr s.
-}
data Expr = BinaryExpr Expr Op Expr | Num Int 
instance Show Expr where
	show (Num x) = show x
	show (BinaryExpr l o r) = "(" ++ show l ++ show o ++ show r ++ ")"

{-
The available mathematical operations.
 -}
data Op = Plus | Minus | Times | Divide 
instance Show Op where
	show Plus =  "+"
	show Minus = "-"
	show Times = "*"
	show Divide = "/"

ops = [Plus, Minus, Times, Divide]

{- 
Evaluates an expression
-}
eval :: Expr -> Int
eval (Num i) = i
eval (BinaryExpr e1 o e2 ) = apply (eval e1) o (eval e2) 

{-
Applies an Operator to two arguments and returns the result
TODO Think of better name
-}
apply :: Int -> Op -> Int -> Int
apply a Plus b = a + b
apply a Minus b = a - b
apply a Times b = a * b
apply a Divide b = a `div` b

{-
Returns true if an operator can be safely applied to the two Integers.
Typically this is to avoid divide by zero errors.
-}
valid :: Int -> Op -> Int -> Bool
valid a Divide b = b /= 0 && a > b && (a `mod` b) == 0
valid a Minus b	= a >= b
valid a o b = True

dedupe :: Int -> Op -> Int -> Bool
dedupe a Times b = a >= b
dedupe a Plus b = a >= b
dedupe a o b = True



asInt c = read c :: Int
asInts = map asInt
