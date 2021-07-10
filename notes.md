# Notes on Learn Haskell for Greater Good

## Chapter 1 - Starting out

The haskell extension is `.hs`

Install `haskell-platform`. It comes with many tools, among them the haskell
compiler `ghc`, and the interpreter `ghci`

Inside `ghci`, you can give it non-haskell commands with `:`. To load a haskell
script named `script.hs`, use `:l script`

Change `ghci`'s prompt with `:set prompt = "string"`

Careful with negative numbers, they can fail in some expresions:
```haskell
5 * - 3  -- This fails
5 * (-3) -- This works
 ```
The booleans are `True` and `False`

The unequal sign is `/=`, not `!=`

### Functions

Define a function as it should be called, but adding an expresion after an `=`
```haskell
doubleMe x = x + x
```
In haskell, function call's have the highest priority among operations. The 
commas and parenthesis common in other programming languages can be ommited.
```cpp
// In C++
function(args)
```
```haskell
-- In haskell
function args
```

Common functions: 
 * `succ int`
 * `min a b` (If `a` and `b` has a well-defined succesor)
 * `max a b` (If `a` and `b` has a well-defined succesor)
 * `print val`
 * `putStr string`

You can also use parenthesis to specify the operator order
```haskell
-- These are equivalent
min min 10 20 max 10 20
min (min 10 20) (max 10 20)
```
You can use \` to design a function with 2 arguments as an infix operator.
```haskell
10 `min` 30
```
An `if` statement in haskell is nothing but an expression. It has 3 required 
fields: `if`, `then` and `else`.
```haskell
doubleSmallNumber = if x > 100 then x else x*2
```
### Lists
Lists are **homogeneous** in haskell. They store several elements of the same
type.

Declaration is done as in python
```haskell
lostNumbers = [4, 8, 15, 16, 23, 42]
```
To concatenate lists, use the `++` operator. Careful, this can be slow.

To add elements to the beginning of a list, use the `:` operator. This is faster

To access elements of the list, use the `!!` operator (Indexed at 0)

Strings are quite literally lists of characters. 
```haskell
['a', 'b', 'c', 'd', 'e'] == "abcde"
```
If a lists' elements are comparable, two of those lists are compared in 
lexicographical order.

Other basic functions:
 * `head ls` (Returns the first element)
 * `tail ls` (Returns everything but the first element)
 * `last ls` (Returns the last element)
 * `init ls` (Returns everything but the last element)  
To sum up:  
   ```
        head                            tail
         |                                |
        /-V-----------------------------------\
        ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐
        └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘
        \-----------------------------------Λ-/
            |                                |
           init                            last
    ```

 * `length ls`
 * `reverse ls`
 * `take num ls` (Returns the `num` first elements)
 * `drop num ls` (Returns the list without the `num` first elements)
 * `maximum ls`
 * `minimum ls`
 * `sum ls` 
 * `elem val ls` (Returns whether `val` is in `ls`)

You can create lists by specifying a range (If the values in that range are
totally ordered) with `..` 
```haskell
[1..12]
```
To specify a step, just separate the first element from the range, and haskell
will infer the step you want it to take. Steps must always be of the same size.
```haskell
[2,4..10]           -- [2,4,6,8,10]
[2,4,6..10]         -- Doesn't work
[1,2,4,8,16..100]   -- Doesn't work
```
We can use infinite lists, since haskell will load them lazily. Using this 
concept, we get the functions:
 * `cycle ls` (Returns a cycled infinite version of ls)
 * `repeat val` (Returns a cycling list of `[val]`)
 * `replicate num val` (Returns a list of `num` elements with value `val`)

#### List comprehensions
We can quickly define lists with list comprehensions. The syntax goes as follows
```haskell
[ function x | x <- domain, predicate x ]
```
The `domain` is the list where the `x` is restrained to. `function` is the
transformation applied to `x`. There is also an optional `predicate` that 
determines if we can use `x` or not.
```haskell
[ x*2 | x <- [1.10], x*2 >= 12]
```
The modulus operator is `mod`, not `%`

There can be as many predicates as you want, separated by commas (Or `&&`)

We can take as many values from llists as we want. Just make sure not to mix the
predicate and domain clauses
```haskell
[ x + y | x <- [1,2,3], y <- [10, 100, 1000] ]
```
### Tuples
They are different from lists in 2 key points:
 * The length is fixed
 * They are heterogeneous (Multiple types can be stored)

Tuples are created with parenthesis, as in python

Unlike lists, you can't compare tuples of different sizes. 

Also, each tuple's type is determined by its length and its element's types, so
a list of tuples of different elements is not possible.

This are some common functions that operate in pairs:
 * `fst pair` (Return first element)
 * `snd pair` (Return second element)

A cool function to create lists of pairs is the `zip` function, which takes a 
list of "first" elements and a list of "second" elements and combines them into
one list of pairs. 

If both lists are of different sizes, the smallest sized is used. Knowing this, 
we can use infinite lists in a `zip` without worry.
```haskell
zip [1..] ["apple", "orange", "cherry", "mango"]
```
> #### Exercise:
> Find a **right** triangle that follows:
>  * The lenghts of the three sides are all integers
>  * The length of each side is less than or equal to 10
>  * The triangle's perimeter (the sum of the side lengths) is equal to 24
> 
> Using list comprehensions
>
> #### My Solution:
> 
> ```haskell
> [(a,b,c) | a <- [1..10], b <- [1..10],c <- [1..10], a + b + c == 24, isRightTriangle a b c]
> isRightTriangle a b c = isTriangle a b c && ( a^2 == abs(b^2 - c^2) || a^2 == b^2 + c^ 2)
> isTriangle a b c = a < b+c && b < a+c && c < a+b
> ```
> 
> #### Solution:
> ```haskell
> [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2+b^2==c^2, a+b+c==24]
> ```
`

## Chapter 2 - Believe the type
To test the type of an expresion, use `:t expr`. 
```haskell
Prelude> :t sum
sum :: (Foldable t, Num a) => t a -> a
```
The `::` is read as "has type of"

The type of a function is (TypeArgs -> TypeRetVal)

Types can be declared explicitely with `::` as follows.
```haskell
functionName :: TypeArgs -> TypeRetVal
```
This is recommended for functions

To represent multiple arguments, think of the functions as if they were 
lambda-calculus functions, which take one argument and return a function
which takes the second and so over.
```haskell
func :: Int -> Int -> Int -> Int
func x y z = x + y + z
```
### Common Haskell Types
 * `Int` Bounded integer
 * `Integer` Unbounded integer
 * `Float`
 * `Double`
 * `Bool`
 * `Char`
 * Tuples, represented as `(Type1, ..., TypeN)`, where `N` is the size of the 
   tuple and `TypeX` is the type of the `X`-th element
 * Lists, represented as `[Type]`, where `Type` is the type of the elements it 
   contains

The same way a function can have variables that represent the arguments of a 
function, type declarations can have _type variables_, with which we can write
generic functions like `head`
```haskell
Prelude> :t head
head :: [a] -> a
```

### Types clases
For functions, we can define constraints for their arguments with typeclasses.
This is better understood with an example. 

> ### Example:
> The `minimum` function can get the minimum value of a list if its elements are
> totally ordered. To specify that, we use the class coinstraint, with `=>`, to
> specify that the arguments of this function must be ordered
> ```haskell
> Prelude> :t minimum
> minimum :: (Foldable t, Ord a) => t a -> a
> ```
`

In this case, both `Foldable` and `Ord` are typeclasses. 

In practice, a type class specifies a bunch of functions that, when we define a
type as an instance of this type class, we must tell what they do for this new
type

### Type classes
#### `Eq` type class
The functions it's instances represent are `==` and `/=`

##### Example:
```haskell
Prelude> :t (==)
(==) :: Eq a => a -> a -> Bool
```

#### `Ord` type class
The functions it's instances represent are `>`, `>=`, `<` and `<=`

##### Example:
```haskell
Prelude> :t compare
compare :: Ord a => a -> a -> Ordering
```
An `Ordering` can be `GT`, `LT` or `EQ`


#### `Show` type class
These type instances can be represented as strings

##### Example:
```haskell
Prelude> :t show
show :: Show a => a -> String
```

#### `Read` type class
These type instances can be defined from strings

##### Example:
```haskell
Prelude> :t read
read :: Read a => String -> a
```
_Just saying `read "4"` in ghci will output an error. This is because it doesn't
know what type do we want the "4" to be transformed to. To solve this, tell the
type to haskell explicitly (`read "4" :: Int`). Usually, this isn't necessary, 
as the type is inferred_

#### `Enum` type class
These type instances are sequential ordered types. They are useful since ranges
can be used with these types

##### Example:
```haskell
Prelude> :t succ
succ :: Enum a => a -> a
```

#### `Bounded` type class
These type instances have a maximum type and a lower type. They can be checked
with the functions `maxBound` and `minBound`

##### Example:
```haskell
Prelude> :t minBound
minBound :: Bounded a => a
```

#### `Num` type class
These type instances act like numbers, and must be instances of `Eq` and `Show`.

##### Example:
```haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

#### `Floating` type class
These type instances include `Double` and `Float`, used to store floating-point 
numbers.

##### Example:
```haskell
Prelude> :t sin
sin :: Floating a => a -> a
```

#### `Integral` type class
These type instances include `Int` and `Integer`, used to store whole numbers.

##### Example:
```haskell
Prelude> :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b
```

#### Chapter 3 - Syntax in functions

