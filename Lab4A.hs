-- Authors: Nicklas Botö, Carl Wiede, Adam Ryden
-- Date:

import Poly
import Test.QuickCheck


-- Use the following simpllowing simple data type for binary operators
data BinOp = AddOp | MulOp

------------------------------------------------------------------
-- A1 ( Define a data type Expr which represents three kinds of expression:
-- binary operators (use BinOp as a helper type) applied to two expressions,
-- numbers (use Int), and exponentiation x^n.
-- Note that since we consider expressions containing just a single variable, x,
-- your data type should not use String or Char anywhere, since this is not needed.

data Expr = Num Int
          | Exponent Int
          | Op BinOp Expr Expr

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

  -- TODO: Om man ska ha kvar caset med t.ex. expr * 0, se över
        -- parenteser på enskilda Ints

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


------------------------------------------undefined------------------------
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

{-

polyToExpr :: Poly -> Expr
polyToExpr poly
  | toList poly          == [] = Num 0
  | length (toList poly) == 1  = Num (head $ toList poly)
  | head(toList poly)    == 0  = polyToExpr(fromList(tail(toList poly)))
  | otherwise                  = Op AddOp (Op MulOp (Num (head(toList poly))) (Exponent (length(toList poly)-1)))
                                          (polyToExpr(fromList(tail(toList poly))))
-}

{- Här har du den som är fin, ditt lilla glin
listToExpr :: [Int] -> Expr
listToExpr []     = Num 0
listToExpr (p:[]) = Num p
listToExpr (0:ps) = polyToExpr $ fromList ps
listToExpr (p:ps) = Op AddOp (Op MulOp (Num p) (Exponent $ (length (p:ps))-1)) (polyToExpr $ fromList ps)

polyToExpr :: Poly -> Expr
polyToExpr poly = listToExpr $ toList poly
-}

polyToExpr :: Poly -> Expr
polyToExpr poly
  | pss        == []                 = Num 0
  | length (pss) == 1                = Num p
  | (p > 1 || p < 0) && all (==0) ps = Op MulOp (Num p) (Exponent (length(pss)-1))
  | (p > 1 || p < 0)                 = Op AddOp (Op MulOp (Num p) (Exponent (length(pss)-1)))
                                         (polyToExpr(fromList ps))
  | p == 1 && all (==0) ps           = Exponent (length(pss)-1)
  | p == 1                           = Op AddOp (Exponent (length(pss)-1)) (polyToExpr(fromList ps))
  | otherwise                        = polyToExpr(fromList ps)

    where
      pss = toList poly
      p = head $ pss
      ps = tail $ pss
      

prop_polyToExpr :: Int -> Poly -> Bool
prop_polyToExpr n poly = evalPoly n poly == eval n (polyToExpr poly)

  
------------------------------------------------------------------
--v-- A8 write a function

{-
write a function simplify :: Expr -> Expr which simplifies an 
expression by converting it to a polynomial and back again (this is easy).
-}

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
prop_noJunk expr = undefined

------------------------------------------------------------------
