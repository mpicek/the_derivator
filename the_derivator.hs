-- Author:  Martin Picek
-- Program: THE DERIVATOR

-- TODO: Suma vice clenu (tzn. seznam [FunctionOP])
-- TODO: Pri vypisovani derivace asi evaluovat vse, co jde (napr n - 1) bude
--       typicky treba (3 - 1), tak tam proste dat 2
-- TODO udelat nezavisly na Double

data FunctionOP = Const Double
                | Var
                | Sum FunctionOP FunctionOP
                | Mul FunctionOP FunctionOP
                | Poly FunctionOP -- for example x^3 .. no Muliplier
                -- deriving (Show)

class Differentiable a where
    derivative :: a -> a

class Evaluate a where
    eval :: Double -> a -> Double


-- TODO: tackle the problem of equality of Var and Poly 1
instance Differentiable FunctionOP where
    derivative (Const _)  = Const 0 -- c' = 0
    derivative Var        = Const 1 -- x' = 1

    -- (f + g)' = f' + g'
    -- derivative (Sum (Const x) (Const y)) = 
    derivative (Sum x y)  = Sum (derivative x) (derivative y)

    -- (fg)' = f'g + fg'
    derivative (Mul x y) = Sum (Mul (derivative x) y) (Mul x (derivative y))

    -- derivative (Poly c 0) = Poly (Mul c n) (Sum n (Const (-1)))
    derivative (Poly (Const 0)) = Const 0  -- ~ Const 1
    derivative (Poly (Const 1)) = Const 1  -- ~ Var
    derivative (Poly n) = Mul n (Poly (Sum n (Const (-1))))

instance Evaluate FunctionOP where
    eval _ (Const c)  = c
    eval x Var        = x
    eval x (Mul y z)  = eval x y * eval x z
    eval x (Sum y z)  = eval x y + eval x z
    eval x (Poly n)   = x ** eval x n

instance Show FunctionOP where
    show (Const x)  = show x
    show Var        = "x"

    show (Sum (Const 0) (Const 0)) = ""
    show (Sum (Const 0) x) = show x
    show (Sum x (Const 0)) = show x

    show (Sum (Const x) (Const y)) = show (x + y)
    show (Sum x y)  = show x ++ " + " ++ show y

    show (Mul (Const 0) _) = ""
    show (Mul _ (Const 0)) = ""
    show (Mul (Const 1) x) = show x
    show (Mul x (Const 1)) = show x
    show (Mul x (Poly y)) = show x ++ "*" ++ show (Poly y)
    show (Mul x y) = show x ++ " * " ++ show y

    show (Poly (Const n)) = "x^" ++ show n
    show (Poly n)   = "x ^ (" ++ show n ++ ")"


