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

Pattern matching is used to specify patterns to which some data should conform
and to deconstruct the data according to those patterns

```haskell
sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
```

The pattern matching is done by checking for each pattern as it was defined.  If
we put the last line second, this function would constantly output `"Not between 1 and 5"`, as no other pattern will be found.

Another clean example:
```haskell
factorial :: Integral -> Integral
factorial 0 = 1
factorial n = n * factorial (n-1)
```

You can 'unpack' tuples like in JavaScript using pattern matching
```haskell
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1+x2, y1+y2)
```

You can use `_` to denote a generic variable. This can be useful when in a 
function, you only need some specific part of a tuple. For example:
```haskell
third :: (a, b, c) -> a
third (_, _, z) = z
```
This can also be used in list comprehensions
```haskell
[ a + b | (a,b) <- pairs ]
```

You can also match with lists, remembering that `[1,2,3]` is just syntactic 
sugar for `1:2:3:[]`
```haskell
head' :: [a] -> a
head' (x:_) = x
```

**If we want to bind something to several values, we must use `()` so that 
haskell can correctly parse them**

```haskell
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element:" ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is too long. The first two elements are: " 
                ++ show x ++ " and " ++ show y
```

We can use as-patterns to break up an item while keeping a reference to the 
entire original item
```haskell
firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter text@(x:_) = "The first letter of " ++ text ++ " is " ++ [x]
```

#### Guards

A guard is indicated by a `|`, and it can be used to pattern match with 
predicates. It basically works as an if statement
```haskell
bmiTell :: Double -> String
bmiTell bm
	| bmi <= 18.5 = "You're underweight, eat more!"
	| bmi <= 25.0 = "Looking good!"
	| bmi <= 30.0 = "You're overweight. Let's work out together!"
	| otherwise = "You're obese. Go see a doctor!"
```
Guard must be indented by at least one space

The `otherwise` keyword functions as a catch-all. It can be substituted by a 
`True` and the effects would stay the same.

#### `where`
We can add a `where` keyword after the guard to declare "local variables", to 
avoid calculating a certain value multiple times
```haskell
bmiTell :: Double -> Double -> String
bmiTell weight height
	| bmi <= skinny = "You're underweight, eat more!"
	| bmi <= normal = "Looking good!"
	| bmi <= fat    = "You're overweight. Let's work out together!"
	| otherwise     = "You're obese. Go see a doctor!"
	where bmi = weight / height^2
		  skinny = 18.5
		  normal = 25.0
		  fat = 30.0
```

You can pattern match in the `where` clause
```haskell
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
	where (f:_) = firstname
		  (l:_) = lastname
```

Functions can also be defined in `where` blocks

#### `let`

The `let` expressions take the form of `let <bindings> in <expression>`
```haskell
cylinder_area :: Double -> Double -> Double
cylinder_area r h = 
	let sideArea = 2 * pi * r * h
		topArea = pi * r^2
	in  sideArea + 2 * topArea
```
The difference between the `let` expressions and `where` blocks are that `let`
expressions are expressions, which means that they can be used outside of a 
function definition.

However, they also can't be used across guards (where the `where` clause is used
instead)

We can use a `let` expression (without the `in` part) inside a list 
comprehension, as if it where a predicate, to create bindings inside the list 
comprehension
```haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis bmis = [bmi | (weight, height) <- bmis, let bmi = weight / height ^2 ]
```

#### `case`

Basically, like a `switch` case on other languages
```haskell
case <expression> of <pattern> -> <result>
                     <pattern> -> <result>
                     <pattern> -> <result>
					 ...
```
The first pattern that matches the expression is used
```haskell
describeList :: [a] -> String
describeList ls = "This list is " ++ case ls of [] -> "empty"
                                                [x] -> "a singleton list"
                                                xs -> "a longer list"
                                  ++ "."
```

> #### Note
> This could also be achieved using pattern matching in a where clause
> ```haskell
> describeList :: [a] -> String
> describeList ls = "The list is " ++ what ls ++ "."
>     where what [] = "empty"
>           what [x] = "a singleton list"
>           what xs = "a longer list"
> ```
`

## Chapter 4 - Recursion
```haskell
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [a] = a
maximum' (x:rest) = x `max` maximum' rest
```
```haskell
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x:replicate (n-1) x
```
```haskell
take' :: Int -> [a] -> [a]
take' n ls
    | n < 0          = error "n can't be negative"
	| n > length ls  = error "n is too large" -- Se raya si ls es infinita
	| n == 0         = []
	| otherwise      = first:take' (n-1) rest
	where (first:rest) = ls
```
```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (first:rest) = reverse' rest ++ first
```
```haskell
repeat' :: a : [a]
repeat' x = x:repeat' x
```
```haskell
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (a:as) (b:bs) = (a,b):zip' as bs
```
```haskell
elem' :: a -> [a] -> Bool
elem' a [] = False
alem' a (first:rest) = a == first || a `elem'` rest
```
```haskell
quicksort' :: [a] -> [a]
quicksort' [] = []
quicksort' (pivot:rest) = quicksort' smallerOrEqual ++ pivot ++ quicksort' greater
    where smallerOrEqual = [x | x <- rest, x <= pivot]
	      greater        = [x | x <- rest, x >  pivot]
```

## Chapter 4 - Higher-order functions

Functions in haskell are curried. This means that they only take one argument,
and return a function that takes the next argument and so on. This means that
we can use these intermediate functions.
```haskell
function :: Int -> Int -> Int
function x y = x + y
function' = function 10
```
In this case, `function'` is a partially applied function of `function`

Infix functions can also be partially applied by using _sections_ by surrounding
it with parenthesis and then supplying the rest of the arguments
```haskell
Prelude> let div10 = (/10)
Prelude> div10 200
20.0
Prelude> let twoHundredDiv = (200/)
Prelude> twoHundredDiv 10
20.0
```
The only gotcha with sections is if you want to use it with `-`. In that case,
`(-4)` means negative 4. You can rewrite that with `substract`
```haskell
applyTwice :: (a->a) -> a -> a
applyTwice f x = f (f x)
```
```haskell
*Main> applyTwice (3/) 10
10.0
*Main> applyTwice (3:) [1]
[3,3,1]
```
### The Functional Programmer's Toolbox
`zipWith` is a function in the standard library which takes two lists and a
function and applies said function to each pair of elements, and adds the 
result to a list
```haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = (f a b):zipWith' as bs
```
`flip` takes a function with two arguments and returns a function that behaves
the same as the provided one, but with it's arguments flipped.
```haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
```
_This is a great example of how curried functions can be really useful in 
haskell_

`map` takes a function and a list and for each element applies the function to
it and returns it finally as a list of results.
```haskell
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
```

`filter` takes a predicate and a list and returns the subsequence of elements
that follow the predicate
```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
-- filter' p (x:xs) = if p x then [x] else [] ++ filter' p xs
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs
```
> ### Example:
> A Collatz sequence or chain is defined as follows:
>  + Start with any natural number
>  + If the number is 1, stop
>  + If the number is even, divide it by 2
>  + If the number is odd, multiply by 3 and add 1
>  + Repeat the algorithm with the resulting number
> 
>  This is theorized to always terminate
> 
>  *For all starting numbers between 1 and 100, how many Collatz
>  chains have a length greater than 15?*
> 
> ```haskell
> length_collatz :: Int -> Int
> length_collatz 1 = 1
> -- length_collatz x
>     -- | x `mod` 2 == 0 = length_collatz (x/2) + 1
> 	-- | otherwise      = length_collatz (3*x+1) + 1
> length_collatz x = 
>     length_collatz (if x `mod`2 == 0 then (x/2) else (3*x + 1)) + 1
> length [x | x <- [1..100], length_collatz x > 15]
> ```
`

### Lambdas

Lambdas are anonymous functions declared to be used once. Their syntax is
```haskell
\ args -> ret_expr
```
We use `\` because it _kinda_ looks like a lambda `λ`

Usually we surround them in parenthesis

### Folds

They are a way to implement a function where you traverse all the items in a 
function one by one and return something based on that. It works like the 
function `std::accumulate` in C++.

You can fold from the left (`foldl`) or right (`foldr`)
```haskell
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl function accumulator list
```
A list is and instance of `Foldable`
```haskell
sum' :: Num n => [n] -> n
sum' ls = foldl (+) 0 ls
-- Viva Curry
sum' = foldl (+) 0
```
###### _`foldlr`_:
```haskell
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) [] 
```
###### _`foldll`_:
```haskell
elem' :: a -> [a] -> Bool
elem' a = foldl (\acc x -> a == x || acc) Fase
```

`foldl1` and `foldr1` are both like `foldl` and `foldr`, but assuming that the
accumulator is the first or last element of the list respectively
```haskell
maximum' :: Ord a => [a] -> a
maximum' = foldl1 max
```
#### Examples of standard library functions implemented with folds
```haskell
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- reverse' = foldl (flip (:)) []
```
```haskell
product' :: Num a => [a] -> a
product' = foldl (*) 1
```
```haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\acc x -> if p x then (x:acc) else acc) []
```
```haskell
last' :: [a] -> a
last' = foldr1 (\_ acc -> acc)
last' = foldl1 (\_ x -> x)
```
Folds can be visualized as follows:
 + `foldr f z [A,B,C,D]`  
   ```
   f A (f B (f C (f D z)))
   ```
 + `foldl g z [A,B,C,D]`  
   ```
   g (g (g (g z A) B) C) D
   ```
Both `foldr` (and `foldl`) will work on infinite lists when the binary function 
that we are passing doesn't always need to evaluate the second parameter (The 
accumulator) to give us some sort of answer

`scanl` and `scanr` are like `foldl` and `foldr` but returning a list with the
intermediate states instead of just the final element.
```haskell
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
```

### Function application 
```haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```
While ` ` is left associative function application, the `$` operator is right
associative function application. Also, ` ` has the highest precedent, but `$`
has the lowest.

`$` can be used to get rid of parenthesis 
```haskell
-- sum (map sqrt [1..130])
sum $ map sqrt [1..130]
```
Also, `$` let's us treat function application like any other operation
```haskell
map ($3) [ (4+), (10*), (^2), sqrt]
```

### Function composition
```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x) = \x -> f $ g x
```
`.` is right associative function composition, so
```
f (g (h x)) == (f . g . h) x
```
This can be useful for chaining functions without using lambdas  
`sum (replicate 5 (max 6.7 8.9))`→`(sum . replicate 5) max 6.7 8.9`→  
→`sum . replicate 5 $ max 6.7 8.9`_(→`sum $ replicate 5 $ max 6.7 8.9`)_

Also, this let's use write functions in _point-free style_ (Taking advantage of
curried functions), like this:
`fn x = ceiling (negate (tan (cos (max 50 x))))` → `fn x = ceiling . negate . 
tan . cos . max 50`  
This is usually more concise

However, making long lists of composition is usually discouraged. Take this into
account for complex functions. It's better to define intermediate functions in a
`let` expression.
