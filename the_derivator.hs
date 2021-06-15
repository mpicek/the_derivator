-- Author:  Martin Picek
-- Program: THE DERIVATOR

-- TODO: Suma vice clenu (tzn. seznam [FunctionOP])
-- TODO: Pri vypisovani derivace asi evaluovat vse, co jde (napr n - 1) bude
--       typicky treba (3 - 1), tak tam proste dat 2
-- TODO udelat nezavisly na Double

import Debug.Trace
debug = flip trace

data FunctionOP = Const Double
                | X
                | Sum FunctionOP FunctionOP
                | Mul FunctionOP FunctionOP
                | Poly FunctionOP FunctionOP -- for example (?)^3
                | Exp FunctionOP
                | ExpCustom FunctionOP FunctionOP
                | Ln FunctionOP
                | Log FunctionOP FunctionOP

class Differentiable a where
    derivative :: a -> a

class Evaluate a where
    eval :: Double -> a -> Double

instance Num FunctionOP where
    (Const a) + (Const b) = Const (a + b)
    a + b                 = Sum a b
    (Const a) - (Const b) = Const (a - b)
    a - (Const b)         = Sum a (Const (-b))
    a - b                 = Sum a (Mul (Const (-1)) b)
    (Const a) * (Const b) = Const (a * b)
    a * b                 = Mul a b
    negate (Const a)      = Const (-a)
    negate a              = Mul (Const (-1)) a
    abs (Const a)         = if a >= 0 then Const a else Const (-a)
    abs a                 = error "No absolute value"
    signum (Const a)      = if a >= 0 then Const 1 else Const (-1)
    signum _              = error "No signum function"
    fromInteger a         = Const (fromIntegral a) -- JESTE SI PROJIT


-- cleanExpr expr: cleans the expression = makes it more readable
cleanExpr :: FunctionOP -> FunctionOP
cleanExpr X = X
cleanExpr (Sum (Const 0) (Const 0)) = Const 0       -- 0 + 0 = 0
cleanExpr (Sum (Const 0) x) = x                     -- 0 + x = x
cleanExpr (Sum x (Const 0)) = x                     -- x + 0 = x
cleanExpr (Sum (Const x) (Const y)) = Const (x + y) -- c1 + c2 = c3
cleanExpr (Mul (Const 0) _) = Const 0               -- 0*_ = 0
cleanExpr (Mul _ (Const 0)) = Const 0               -- _*0 = 0
cleanExpr (Mul (Const 1) x) = x                     -- 1*x = x
cleanExpr (Mul x (Const 1)) = x                     -- x*1 = x
cleanExpr (Mul (Const a) (Const b)) = Const (a * b)
cleanExpr (Poly _ (Const 0)) = Const 1              -- x^0 = 0
cleanExpr x = x


instance Differentiable FunctionOP where
    derivative X = Const 1
    derivative (Const _)  = Const 0 -- c' = 0

    -- (f + g)' = f' + g'
    derivative (Sum x y)  = derivative x + derivative y

    derivative (Mul (Const x) y) = Mul (Const x) (derivative y)
    derivative (Mul x (Const y)) = Mul (Const y) (derivative x)

    -- (fg)' = f'g + fg'
    derivative (Mul x y) = (derivative x * y) + (x * derivative y)

    -- derivative (Poly c 0) = Poly (Mul c n) (Sum n (Const (-1)))
    derivative (Poly x (Const 0)) = Mul (Const 0) (derivative x) -- chain rule
    derivative (Poly x (Const 1)) = Mul (Const 1) (derivative x) -- chain rule
    derivative (Poly x n) = Mul (Mul n (Poly x (Sum n (Const (-1))))) (derivative x) -- chain rule

    derivative (Exp x) = Mul (Exp x) (derivative x) -- chain rule
    derivative (ExpCustom a x) = Mul (Mul (ExpCustom a x) (Ln a)) (derivative x) -- (5^x)' = 5^x*ln5 * x'

    derivative (Ln x) = Mul (Poly x (Const (-1))) (derivative x)

    -- TODO: derivace Log x a

    -- Differentiating x
    -- (log_a(x))' = (1/(ln(a) * x)) * x'
    derivative (Log a x) = Mul (Poly (Mul (Ln a) x) (Const (-1))) (derivative x) -- chain rule


instance Evaluate FunctionOP where
    eval x X = x
    eval _ (Const c)       = c
    eval x (Mul y z)       = eval x y * eval x z
    eval x (Sum y z)       = eval x y + eval x z
    eval x (Poly a n)      = eval x a ** eval x n
    eval x (Exp y)         = exp (eval x y)
    eval x (ExpCustom a y) = eval x a ** eval x y
    eval x (Ln y)          = log (eval x y)
    eval x (Log a b)       = logBase (eval x a) (eval x b)

instance Show FunctionOP where
    show X = "x"
    show (Const x)  = show x

    -- show (Sum (Const 0) (Const 0)) = ""
    -- show (Sum (Const 0) x) = show x
    -- show (Sum x (Const 0)) = show x

    -- show (Sum (Const x) (Const y)) = show (x + y)
    show (Sum x y)  = "(" ++ show x ++ " + " ++ show y ++ ")"

    -- show (Mul (Const 0) _) = ""
    -- show (Mul _ (Const 0)) = ""
    -- show (Mul (Const 1) x) = show x
    -- show (Mul x (Const 1)) = show x
    show (Mul x (Poly a y)) = show x ++ " * " ++ show (Poly a y)
    show (Mul x y) = show x ++ " * " ++ show y

    show (Poly a (Const 1)) = show a
    show (Poly a (Const n)) = "(" ++ show a ++ ")" ++ "^" ++ show n
    show (Poly a n)   = "(" ++ show a ++ ")^(" ++ show n ++ ")"


