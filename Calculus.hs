module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger 
  -- negate (Val 0.0)  =  Val 0.0
  negate x     = UnApp Neg x
  -- (+) x 0   = x
  -- (+) 0 x   = x 
  (+) x y      = BinApp Add x y
  -- (*) x 1   = x
  -- (*) 1 x   = x
  (*) x y      = BinApp Mul x y
   
-- Leave the following two undefined...
  signum       = undefined
  abs          = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  -- (/) 0 _   = Val 0.0
  -- (/) x 1   = x 
  (/) x y      = BinApp Div x y

-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

-- Pre : Key should have a value in the list
lookUp :: Eq a => a -> [(a, b)] -> b
--lookUp _ [] = error " Empty List"
lookUp k keyVals = fromJust (lookup k keyVals)

showExp :: Exp -> String
showExp (Val x) = show x
showExp (Id x) = x
showExp (UnApp op x)
  = f ++ "(" ++ (showExp x) ++ ")"
  where
    f = lookUp op [(Neg, "-"), (Sin, "sin"), (Cos, "cos"), (Log, "log")]
showExp (BinApp op x x')
  =  "(" ++ (showExp x) ++ f ++ (showExp x') ++ ")"
  where
    f = lookUp op [(Add, "+"),(Mul, "*"), (Div, "/")]

eval :: Exp -> Env -> Double
eval (Val x) _ = x
eval (Id x) env
  = lookUp x env
eval (UnApp op x) env
  = f (eval x env)
  where 
    f  = lookUp op [(Neg, negate), (Sin, sin), (Cos, cos), (Log, log)] 
eval (BinApp op x x') env
  = f (eval x env) (eval x' env)  
  where
    f = lookUp op [(Add, (+)),(Mul, (*)), (Div, (/))]

diff :: Exp -> String -> Exp
diff (Val _) _ = (Val 0.0)
diff (Id x) s
  | showExp (Id x) == s = fromInteger 1
  | otherwise           = fromInteger 0
diff (BinApp Add x x') s = (diff x s) + (diff x' s) 
diff (BinApp Mul x x') s = (x * (diff x' s)) + ((diff x s) * x')
diff (BinApp Div x x') s = (((diff x s) * x') + (negate (x * (diff x' s)))) / (BinApp Mul x' x')
diff (UnApp Sin x) s
  = (cos x) * (diff x s)
diff (UnApp Cos x) s
  = negate ((sin x) * (diff x s))
diff (UnApp Log x) s
  = (diff x s) / x
diff (UnApp Neg x) s
  = negate (diff x s)
   
maclaurin :: Exp -> Double -> Int -> Double
maclaurin e _ 1 = eval e [("x",0)] 
maclaurin e x n 
  = sum (zipWith3 (\x y z -> (x * y) / z) num1 num2 denominator)
  where
    num1 = map (`eval` [("x",0)]) (take n (iterate (`diff` "x") e))
    num2 = zipWith (^) [x | _ <- [1..n]] [0..]
    denominator = map fromIntegral (scanl (*) 1 [1..])

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

e1 = 5 * x
-- e1 = BinApp Mul (Val 5.0) (Id "x")

e2 = x * x + y - 7
-- e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
--                (UnApp Neg (Val 7.0))

e3 = x - y^2 / (4 * x * y - y^2) :: Exp
-- e3 = BinApp Add (Id "x")
--            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
--            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
--                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

e4 = -cos x :: Exp
-- e4 = UnApp Neg (UnApp Cos (Id "x"))

e5 = sin (1+log(2*x)) :: Exp
-- e5 = UnApp Sin (BinApp Add (Val 1.0)
--                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

e6 = log(3*x^2+2) :: Exp
-- e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
--                           (Val 2.0))
