# THE DERIVATOR: Automatic Differentiation in Haskell

**This is a simple application for automatic differentiation in Haskell.**

## Supported functions

**Supports all basic functions:**
  - `Const`                .. a constant
  - `X`                    .. a variable that will be differentiated
  - `Sum a b` (or `a + b`) .. a sum
  - `Mul a b` (or `a * b`) .. a multiplication
  - `Poly x n`             .. `x^n`, `n` doesn't have `x` inside
  - `Exp x`                .. `e^x`
  - `ExpCustom a x`        .. `a^x`
  - `Ln x`                 .. natural logarithm of `x`
  - `Log a x`              .. logarithm of `x` to the base `a`
  - `Sin x`                .. `sin x`
  - `Cos x`                .. `cos x`

## Installation and running the program

Install Haskell on your computer, change directory to this folder and run `ghci`.
Then insert your function and use function `derivative` to differentiate the
function. You can also use function `eval` to evaluate your function (or
its derivative) given `x` of type `Double`.

## Examples

Here is an example of creating and differentiating common functions used
in machine learning - sigmoid and tanh.

```Haskell
λ> sigmoid = Poly (Sum 1 (Exp (Mul (-1) X))) (-1)
λ> sigmoid
((1.0 + e^(-1.0 * x)))^-1.0
λ> derivative sigmoid
-1.0 * ((1.0 + e^(-1.0 * x)))^(-2.0) * e^(-1.0 * x) * -1.0
λ> 
λ> 
λ> tanh = 2 * (Poly (Sum 1 (Exp (Mul (-2) X))) (-1)) - 1
λ> derivative tanh
2.0 * -1.0 * ((1.0 + e^(-2.0 * x)))^(-2.0) * e^(-2.0 * x) * -2.0
λ> 
λ> eval 1 (derivative sigmoid)
0.19661193324148188
λ> 
λ> eval 2.5 tanh
0.9866142981514305
```

![derivator](https://static01.nyt.com/images/2020/08/10/arts/31comfort-terminator4/31comfort-terminator4-facebookJumbo.jpg)
