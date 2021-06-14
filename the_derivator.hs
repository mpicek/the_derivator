-- Author:  Martin Picek
-- Program: THE DERIVATOR

-- TODO: Suma vice clenu (tzn. seznam [FunctionOP])
-- TODO: Pri vypisovani derivace asi evaluovat vse, co jde (napr n - 1) bude
--       typicky treba (3 - 1), tak tam proste dat 2
-- TODO udelat nezavisly na Double
-- TODO chain rule

data FunctionOP = Const Double
                | Sum FunctionOP FunctionOP
                | Mul FunctionOP FunctionOP
                | Poly FunctionOP -- for example x^3 .. no Muliplier

                | Exp FunctionOP
                | ExpCustom FunctionOP FunctionOP
                | Recip FunctionOP
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
    a * b                 = Sum a b
    negate (Const a)      = Const (-a)
    negate a              = Mul (Const (-1)) a
    abs (Const a)         = if a >= 0 then Const a else Const (-a)
    abs a                 = error "TO BE DONE"
    signum (Const a)      = if a >= 0 then Const 1 else Const (-1)
    signum _              = error "TO BE DONE"
    fromInteger a         = Const (fromIntegral a) -- JESTE SI PROJIT

    
-- cleanExpr expr: cleans the expression = makes it more readable
cleanExpr :: FunctionOP -> FunctionOP
cleanExpr (Sum (Const 0) (Const 0)) = Const 0       -- 0 + 0 = 0
cleanExpr (Sum (Const 0) x) = x                     -- 0 + x = x
cleanExpr (Sum x (Const 0)) = x                     -- x + 0 = x
cleanExpr (Sum (Const x) (Const y)) = Const (x + y) -- c1 + c2 = c3
cleanExpr (Mul (Const 0) _) = Const 0               -- 0*_ = 0
cleanExpr (Mul _ (Const 0)) = Const 0               -- _*0 = 0
cleanExpr (Mul (Const 1) x) = x                     -- 1*x = x
cleanExpr (Mul x (Const 1)) = x                     -- x*1 = x
cleanExpr (Poly (Const 0)) = Const 1                -- x^0 = 0


instance Differentiable FunctionOP where
    derivative (Const _)  = Const 0 -- c' = 0

    -- (f + g)' = f' + g'
    -- derivative (Sum (Const x) (Const y)) = 
    derivative (Sum x y)  = Sum (derivative x) (derivative y)

    -- (fg)' = f'g + fg'
    derivative (Mul x y) = Sum (Mul (derivative x) y) (Mul x (derivative y))

    -- derivative (Poly c 0) = Poly (Mul c n) (Sum n (Const (-1)))
    derivative (Poly (Const 0)) = Const 0
    derivative (Poly (Const 1)) = Const 1
    derivative (Poly n) = Mul n (Poly (Sum n (Const (-1))))

instance Evaluate FunctionOP where
    eval _ (Const c)  = c
    eval x (Mul y z)  = eval x y * eval x z
    eval x (Sum y z)  = eval x y + eval x z
    eval x (Poly n)   = x ** eval x n

instance Show FunctionOP where
    show (Const x)  = show x

    -- show (Sum (Const 0) (Const 0)) = ""
    -- show (Sum (Const 0) x) = show x
    -- show (Sum x (Const 0)) = show x

    -- show (Sum (Const x) (Const y)) = show (x + y)
    show (Sum x y)  = show x ++ " + " ++ show y

    -- show (Mul (Const 0) _) = ""
    -- show (Mul _ (Const 0)) = ""
    -- show (Mul (Const 1) x) = show x
    -- show (Mul x (Const 1)) = show x
    show (Mul x (Poly y)) = show x ++ "*" ++ show (Poly y)
    show (Mul x y) = show x ++ " * " ++ show y

    show (Poly (Const 1)) = "x"
    show (Poly (Const n)) = "x^" ++ show n
    show (Poly n)   = "x ^ (" ++ show n ++ ")"


