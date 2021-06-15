-- Author:  Martin Picek
-- Program: THE DERIVATOR

-- TODO: derivace Log x a (jakoze derivace toho x)
-- TODO: derivace x^x
-- TODO: derivace Log x x (jako v obojim muze bejt x, neni to jen primo Log x x)

{-
DOCUMENTATION

FunctionOP - a data type that holds all kinds of functions.
These functions should have derivatives. Therefore, there is a class Differentiable
that ensures that its instances have a derivation. This is why we need to
create an instance of Differentiable for FunctionOP data type.

We also provide Num instance for FunctionOP data type. It makes
it way more easier for us to read and parse expressions from the input. It is
what the Num class provides for us.
  - Extra note: function fromInteger gets an Integer and does something with that
      -> "does something" = we create a constant Const x

Class Evaluatable provides us with eval function that replaces all Xs by
a Double and evaluates the expression.

Finally, we create an instance of the class Show, so that we print the expressions
more nicely.
-}

data FunctionOP = Const Double
                | X
                | Sum FunctionOP FunctionOP
                | Mul FunctionOP FunctionOP
                | Poly FunctionOP FunctionOP -- for example (?)^3 (X is not in an exponent)
                | Exp FunctionOP             -- X in exponent
                | ExpCustom FunctionOP FunctionOP
                | Ln FunctionOP
                | Log FunctionOP FunctionOP
                | Sin FunctionOP
                | Cos FunctionOP


class Differentiable a where
    derivative :: a -> a

class Evaluateable a where
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


-- In the following differentiation rules, there is always a chain rule used.
-- Most of the rules are therefore composed of Mul, where the second factor is a
-- recursive derivative from the chain rule.
instance Differentiable FunctionOP where
    derivative X                  = Const 1
    derivative (Const _)          = Const 0 -- c' = 0

    -- (f + g)' = f' + g'
    derivative (Sum x y)          = derivative x + derivative y

    -- edge cases for Mul
    derivative (Mul (Const x) y)  = Mul (Const x) (derivative y)
    derivative (Mul x (Const y))  = Mul (Const y) (derivative x)

    -- (fg)' = f'g + fg'
    derivative (Mul x y)          = (derivative x * y) + (x * derivative y)

    -- edge cases for Poly
    derivative (Poly x (Const 0)) = Mul (Const 0) (derivative x)
    derivative (Poly x (Const 1)) = Mul (Const 1) (derivative x)
    derivative (Poly x n)         = Mul (Mul n (Poly x (Sum n (Const (-1))))) (derivative x)

    derivative (Exp x)            = Mul (Exp x) (derivative x)
    -- differentiating x (not a) ... (5^x)' = 5^x*ln5 * x'
    derivative (ExpCustom a x)    = Mul (Mul (ExpCustom a x) (Ln a)) (derivative x)

    derivative (Ln x)             = Mul (Poly x (Const (-1))) (derivative x)

    -- Differentiating x (not a)
    -- (log_a(x))' = (1/(ln(a) * x)) * x'
    derivative (Log a x)          = Mul (Poly (Mul (Ln a) x) (Const (-1))) (derivative x) -- chain rule

    derivative (Sin x)            = Mul (Cos x) (derivative x)
    derivative (Cos x)            = Mul (Mul (Sin x) (Const (-1))) (derivative x)


instance Evaluateable FunctionOP where
    eval x X = x
    eval _ (Const c)       = c
    eval x (Mul y z)       = eval x y * eval x z
    eval x (Sum y z)       = eval x y + eval x z
    eval x (Poly a n)      = eval x a ** eval x n
    eval x (Exp y)         = exp (eval x y)
    eval x (ExpCustom a y) = eval x a ** eval x y
    eval x (Ln y)          = log (eval x y)
    eval x (Log a b)       = logBase (eval x a) (eval x b)
    eval x (Sin a)         = sin (eval x a)
    eval x (Cos a)         = cos (eval x a)


instance Show FunctionOP where
    show X                         = "x"
    show (Const x)                 = show x

    show (Sum (Const 0) (Const 0)) = ""
    show (Sum (Const 0) x)         = show x
    show (Sum x (Const 0))         = show x
    show (Sum (Const a) (Const b)) = show (Const (a + b))

    show (Sum x y)                 = "(" ++ show x ++ " + " ++ show y ++ ")"

    show (Mul (Const 0) _)         = ""
    show (Mul _ (Const 0))         = ""
    show (Mul (Const 1) x)         = show x
    show (Mul x (Const 1))         = show x
    show (Mul (Const a) (Const b)) = show (Const (a * b))

    show (Mul x (Poly a y))        = show x ++ " * " ++ show (Poly a y)
    show (Mul x y)                 = show x ++ " * " ++ show y

    show (Poly X (Const x))        = "x^" ++ show x
    show (Poly X x)                = "x^(" ++ show x ++ ")"
    show (Poly a (Const 1))        = show a
    show (Poly a (Const n))        = "(" ++ show a ++ ")" ++ "^" ++ show n
    show (Poly a n)                = "(" ++ show a ++ ")^(" ++ show n ++ ")"

    show (Sin x)                   = "sin(" ++ show x ++ ")"
    show (Cos x)                   = "cos(" ++ show x ++ ")"
    


