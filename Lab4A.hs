-- Authors: Nicklas Botö, Carl Wiede, Adam Ryden
-- Date: 2018-10-19

import Poly
import Test.QuickCheck


-- Use the following simpllowing simple data type for binary operators
data BinOp = AddOp | MulOp
  deriving (Eq)

------------------------------------------------------------------
-- A1 ( Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable, x,
-- your data type should not use String or Char anywhere, since this is not needed.

data Expr = Num Int
          | Exponent Int
          | Op BinOp Expr Expr
  deriving (Eq)

------------------------------------------------------------------
-- A2  Define the data type invariant that checks that exponents are never negative

prop_Expr :: Expr -> Bool
prop_Expr (Num _)                 = True
prop_Expr (Exponent n)            = n >= 0
prop_Expr (Op AddOp expr1 expr2)  = prop_Expr expr1 && prop_Expr expr2
prop_Expr (Op MulOp expr1 expr2)  = prop_Expr expr1 && prop_Expr expr2



------------------------------------------------------------------
-- A3 Make Expr an instance of Show (along the lines of the example in the lecture).
-- You can use Haskell notation for powers: x^2
-- You should show x^1 as just x.

instance Show Expr where
  show (Op AddOp expr1 expr2)     = show expr1 ++ " + " ++ show expr2
  show (Op MulOp expr1 expr2)     = showFactor expr1 ++ " * " ++ showFactor expr2
  show (Num n)                    = show n
  show (Exponent 0)               = "1"
  show (Exponent 1)               = "x"
  show (Exponent expo)            = "x^" ++ show expo


-- showFactor is used to display parentheses around
-- multiplicative expressions in a correct manner

showFactor (Op AddOp e1 e2) = "(" ++ show (Op AddOp e1 e2) ++ ")"
showFactor e           = show e

------------------------------------------------------------------
-- A4 Make Expr and instance of Arbitrary.
-- Now you can check the data type invariant that you defined in A3 using quickCheck

-- (Optional) Add a definition of function shrink :: Expr -> [Expr] to Arbitrary
-- which gives hints to quickCheck on possible smaller expressions that it could use
-- to find a smaller counterexample for failing tests


instance Arbitrary Expr
  where arbitrary = sized rExpr

rExpr :: Int -> Gen Expr
rExpr s = frequency [(1,rNum),(s,rBin),(s,rExpo)]
    where
    rNum :: Gen Expr
    rNum = do
      n <- arbitrary
      return $ Num n

    rBin :: Gen Expr
    rBin = do
      op <- elements [AddOp, MulOp]
      e1 <- rExpr s'
      e2 <- rExpr s'
      return $ Op op e1 e2
        where s' = s `div` 2

    rExpo :: Gen Expr
    rExpo = do
      expo <- elements [2..9]
      return $ Exponent expo
        where s' = s `div` 2


------------------------------------------------------------------
-- A5 Define the eval function which takes a value for x and an expression and evaluates it

eval :: Int -> Expr -> Int
eval x (Num n)                = n
eval x (Op AddOp expr1 expr2) = eval x expr1 + eval x expr2
eval x (Op MulOp expr1 expr2) = eval x expr1 * eval x expr2
eval x (Exponent e)           = x ^ e


------------------------------------------------------------------
-- A6 Define
-- Which converts an expression into a polynomial.
-- Here it is important to think recursively to just solve the bigger problem
-- by solving the smaller problems and combining them in the right way.

exprToPoly :: Expr -> Poly
exprToPoly (Num n)                = fromList [n]
exprToPoly (Exponent e)           = fromList $ [1] ++ (replicate e 0)
exprToPoly (Op MulOp expr1 expr2) = exprToPoly(expr1) * exprToPoly(expr2)
exprToPoly (Op AddOp expr1 expr2) = exprToPoly(expr1) + exprToPoly(expr2)

prop_exprToPoly :: Int -> Expr -> Bool
prop_exprToPoly n expr = eval n expr == evalPoly n (exprToPoly expr)


-- A7
-- Now define the function going in the other direction, 
-- polyToExpr :: Poly -> Expr. Take care to use “smart constructors”
-- (see lecture week 6 part II)[https://www.youtube.com/watch?v=98e1L4CVXP4] 
-- to ensure that you don’t introduce “junk” like multiplication by 1 in your result.

-- Write (and check) a quickCheck property prop_polyToExpr for this function similar to that for A6.


listToExpr :: [Int] -> Expr
listToExpr []     = Num 0
listToExpr (p:[]) = Num p
listToExpr (0:ps) = polyToExpr $ fromList ps
listToExpr (1:ps)
  | all (==0) ps  = Exponent (length ps)
  | otherwise     = Op AddOp (Exponent (length ps)) (polyToExpr(fromList ps))
listToExpr (p:ps)
  | all (==0) ps  = Op MulOp (Num p) (Exponent (length ps))
  | otherwise     = Op AddOp (Op MulOp (Num p) (Exponent (length ps))) (polyToExpr(fromList ps))


-- polyToExpr turns the input poly into an [Int]
-- for the listToExpr function to make pattern matching available

polyToExpr :: Poly -> Expr
polyToExpr poly = listToExpr $ toList poly

-- prop_polyToExpr tests if the value of the input poly
-- is the same as when it is converted to an expression

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr n poly = evalPoly n poly == eval n (polyToExpr poly)

  
------------------------------------------------------------------
-- A8 write a function simplify :: Expr -> Expr which simplifies an 
-- expression by converting it to a polynomial and back again (this is easy).

simplify :: Expr -> Expr
simplify expr = polyToExpr . exprToPoly $ expr


------------------------------------------------------------------
-- A9 write a function
-- Write a quickCheck property

--that checks that your definition does not contain any "junk":
--where junk is defined to be multiplication by one or zero,
--addition of zero, addition or multiplication of numbers, or x to the
--power zero. (You may need to fix A07)

prop_noJunk :: Expr -> Bool

prop_noJunk (Op AddOp expr1 (Num 0)) = simplify (Op AddOp expr1 (Num 0)) == simplify expr1
prop_noJunk (Op AddOp (Num 0) expr2) = simplify (Op AddOp (Num 0) expr2) == simplify expr2

prop_noJunk (Op MulOp expr1 (Num 0)) = simplify (Op MulOp expr1 (Num 0)) == simplify (Num 0)
prop_noJunk (Op MulOp (Num 0) expr2) = simplify (Op MulOp (Num 0) expr2) == simplify (Num 0)

prop_noJunk (Op MulOp expr1 (Num 1)) = simplify (Op MulOp expr1 (Num 1)) == simplify expr1
prop_noJunk (Op MulOp (Num 1) expr2) = simplify (Op MulOp (Num 1) expr2) == simplify expr2

prop_noJunk (Exponent 0)             = simplify (Exponent 0)             == simplify (Num 1)

prop_noJunk (Exponent e)             = simplify (Exponent e)             == simplify (Exponent e)
prop_noJunk (Num n)                  = simplify (Num n)                  == simplify (Num n)

prop_noJunk (Op MulOp expr1 expr2)   = prop_noJunk expr1 && prop_noJunk expr2
prop_noJunk (Op AddOp expr1 expr2)   = prop_noJunk expr1 && prop_noJunk expr2