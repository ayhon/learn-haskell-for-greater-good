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
The operator priority for the `if` expression is pretty low. This means that all
parts of an expression that are not required by other functions will be absoved
by the `if`, `then` or `else` parts.

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

## Chapter 5 - Recursion
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

## Chapter 6 - Modules

A haskell module is a file that defines some functions, types and type classes.  
A haskell program is a collection of modules

The haskell standard library is separated in many different modules. One such 
module is `Prelude`, which is where all the functions we've used up until now 
are defined.

To import a module, use `import Module` in a script, or `:m + Module1 Module2`
in `ghci`.

You can import specific functions from a module with `import Module (func1, 
func2)`

You cand also hide specific functions from a module by doing `import Module hiding (func1, func2)`

A qualified import makes mandatory to prepend the module name, which helps in 
avoiding name clashes. Use `import qualified Module` to do this.

Some module names are stupidly long, so use `import Module as M` to be able to
refer to it only by `M`.

The difference between `.` for referencing a function from a module and `.` for
function composition are the white-space that must go in between

### Exercises

```haskell
countWords :: String -> [(String, Int)]
countWords = map (\ls@(x:_) -> (x,length ls)) . group . sort . words
```
```haskell
isIn :: Eq a => a -> [a] -> Bool
isIn elem = any (elem `isPrefixOf`) . tails
```
```haskell
ccypher :: String -> Int -> String
-- ccypher msg n =  map chr . map (+n) $ map ord msg
ccypher n = map (\c -> chr $ ord c + n) 
```
```haskell

```
### `Data.List`

 + `nub` Filters a list for unique numbers
 + `words` Given a sentence, return a list of the words in that sentence
 + `group` Given a list, returns a list of grouped elements (Grouped if they are
 equal)  
   ```haskell
   [1,1,1,1,1,1,2,2,2,2,3,3,3,3,4,6,9] → [[1,1,1,1,1,1],[2,2,2,2],[3,3,3,3],[4],[6],[9]]
   ```
 + `sort` Given a list of things that can be ordered and orders them
 + `isPrefixOf` Self-explanatory
 + `tails` Returns a list with the tails called from every position in the list
 + `foldl'` A strict version of `foldl`, which doesn't postpone computations
 + `find` Given a list and a condition, returns the first element which follows
 that condition. It's signature is:
   ```haskell
   find :: (a -> Bool) -> [a] -> Maybe a
   ```
   where the `Maybe` type can have either 0 or 1 element (Like a bounded list).  
   When there is only one element, it's of the type `Just elem`. If there is 
   none, the value is `Nothing`
 + `lookup` Given a key and a a list of pairs of keys and values (association 
 list), returns the value associated with the first key found (linearly)

### `Data.Map`
<!-- It has a filter function as well -->
 + `fromList` Creates a map from an association list
   ```haskell
   fromList :: (Ord k) => [(k, v)] -> Data.Map.Map k v
   ```
 + `fromListWith` Creates a map from an association list and a function, which is
 used to decide what to do with duplicate keys
   ```haskell
   fromListWith :: (Ord k) => [(k, v)] -> (v -> v -> v) -> Data.Map.Map k v
   ```
 + `lookup` Given a key and a Map, returns its value (Or Nothing)
 + `insert` Given a key, a value and a Map with such keys and values, inserts 
 the new pair in the Map
 + `size` Given a map, returns its size (Number of keys)
 + `map` Given a function and a map, apply that function to every value of the
 map

### `Data.Char`

 + `ord` Given a character, returns its ASCII position
 + `chr` Given a number, returns the character with its that in ASCII
 + `digitToInt` Transforms a number into its corresponding character 
 representation

### Making our own modules

You make your own module following the following syntax

```haskell
module ModuleName
( exportedFunction1
, exportedFunction2
, exportedFunction3
, exportedFunction4
, exportedFunction5
) where
```

After the where, we define our own functions

We can also have hierarchical modules, where each module have submodules which
can have submodules of their own. This is done with directories and 
subdirectories, like in Java

## Chapter 7 - Making our own types and type classes

Create types using the `data`keyword
```haskell
data Bool = False | True
```
```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```
The `Shpae` type has two constructors. The `Circle` constructor will take 3
floats, and the `Rectangle` four
```haskell
Circle :: Float -> Float -> Float -> Shape
Rectangle :: Float -> Float -> Float -> Float -> Shape
```
We can pattern match to constructors (As we did with `[]` or `True`)
```haskell
area :: Shape -> Float
area (Circle _ _ r) = 2*pi*r
area (Rectangle x1 y1 x2 y2) = (abs $ x1-x2) * (abs $ y1-y2)
```
We can make `Shape` be a part of the `Show` type class (For example), with the
`deriving (TypeClass)` syntax, so the `Shape` type definition would be:
```haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)
```
Since `Float Float` are used to represent a point, let's declare the point data
type:
```haskell
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi*r^2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1-x2) * (abs $ y1-y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy 
    = Rectangle (Point x1+dx y1+dy) (Point x2+dx y2+dy)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Shape
baseRect h w = Rectangle (Point 0 0) (Point w h)
```
You can export types in a module, just include them as if they were a function, 
and in parenthesis put the names of the constructors you are exporting. To 
export all constructors, use the shorthand `(..)` instead of writing them all.
We could export the type without exporting the constructors by leaving the 
parenthesis empty.

We can define a type the following two ways:
```haskell
--                   Name   Lastnm Age Height Tlf    Icecream flavor
data Person = Person String String Int Float  String String
    deriving (Show)

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavor :: String
                       } deriving (Show)
```

With the last way, the functions `firstName`, `lastName` and so own are also
automatically created
```haskell
firstName :: Person -> String
```
This is called the record syntax. When defined with the record syntax, the
constructor changes to:
```haskell
Person {firstName = "A", lastName = "B", age = 1, height = 1.0,
     phoneNumber = "A ", flavor = "A"}
```
We must name each field, but the order can be changed

A character constructor can take some parameters (Type parameters)
```haskell
data Maybe a = Nothing | Just a
```
The equivalent of "casting" in haskell is done with `::`. `Just 3` by default
is of type `Num a => Maybe a`, but we may want to be more specific, so using 
`::` will make i behave as we want to
```haskell                  
Just 3 :: Maybe Int
```                         
A type is **concrete** if it doesn't take any parameters at all or if it takes
type parameters and this have been all filled up (`Maybe Char`).

`Nothing` is type of `Maybe a`, where `a` is a type variable. This means that
`Nothing` can act as any `Maybe x` for any `x`. It's the same as the empty list,
which is of type `[a]`.

**In general, don't put type constraints (`(___ __) =>`) into data declarations**

If a type derives the `Eq` type class, then it will test equality using the
constructors of both expressions.

You can derive any type whose internal fields are instances of `Eq`

You can derive from `Show` any type, and it'll use its constructor as the 
representation. The same is true (but opposite) for `Read`.

You can call `read` on an instance of `Read`, and it'll return an object. 
However, you must specify to read what that string must be read as (Or put it as
part of an operation that makes sense, so that haskell can infer it from 
context)
```haskell
read :: Read a => String -> a
```
```haskell
read "Person { fistName=\"Michael\", lastName=\"Diamond\", age=43}" :: Person :: Person
```
When deriving from `Ord`, the earlier the constructor was in the beginning of 
the expression, the higher order it gets. Values resulting from the same 
parameters in the constructor are considered equal. If both values come from the
same constructor, the internal fields are compared (So they must be instances of
`Ord`)

If the values of a type are all literals, they can be made instances of `Enum` 
so they'll be usable with functions like `succ` and `pred`.
```haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
```
We could also make it part of `Bounded`

### `type`

You can define synonyms for a type with the expression
```haskell
type String = [Char]
```
_Like `using` in C++_

```haskell
type Name = String
type PhoneNumber = String
type AssocList k v = [(k,v)]
type PhoneBook = AssocList Name PhoneNumber

phoneBook :: PhoneBook
phoneBook = [
    ("betty", "555-2938"),
    ("bonnie", "526-1989"),
    ("patsy", "235-1560"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
    ]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook
```
As you can see, they can also take type arguments

We can use the `Either a b` class to distinguish between two possibilities. It's
defined as follows
```haskell
data Either a = Left a | Right b
    deriving (Eq, Ord, Show, Read)
```
This is usually used to get information about errors in a function. The `Left`
is usually left for errors, while `Right` is used for the result
```haskell
-- A High School has lockers with a code combination. The students ask the 
-- locker supervisor for an specific locker, and the supervisor will either 
-- give them the combination of the locker they chose or ask them to take
-- another locker

import qualified Data.Map as Map

data LockerState = Taken | Free
    deriving (Show, Equ)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup  lockerNumber map = case Map.lookup lockerNumber map of
    Nothing            -> Left $ "Locker" ++ show lockerNumber 
                                 ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken 
                            then Right code 
                            else Left $ "Locker " ++ show lockerNumber 
                                        ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList 
    [ (100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "Q0TSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]
```
We can make data types recursively (Like the list, `[1,2]` is just `1:2:[]`)
```haskell
data List a = Empty | Cons a (List a)
     deriving (Eq, Ord, Show, Read)
```
Here, `Cons` is but another word for `:`.

### Infix functions and fixity

We can define functions to be automatically infix by naming them using only 
special characters. We can do the same with constructors, although infix
constructors must be surrounded by `:`.

Also, we can use `infixr` and `infixl` to define the fixity of our function. It
follows the next expression
```haskell
infixr 5 /\
(/\) :: Num a => a -> a -> a
x /\ y = max x y - min x y
```
```haskell
infixr 5 :-:
data List a = Empty | a :-: (List a)
     deriving (Eq, Ord, Show, Read)

list :: List Int
list = 1 :-: 10 :-: 20 :-: Empty
```

After seeing al of this, it follows that pattern matching works on the form 
(pattern) given by the constructors.

### Defining type classes

This is an example definition of the type class `Eq`, which requires that the
operators `==` and `/=` are defined.
```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```
This recursive definition will be useful

We can define class instances by hand

```haskell
data TrafficLight = Read | Yellow | Green

instance Eq TrafficLight where
    Red == Red       = True
    Green == Green   = True
    Yellow == Yellow = True
    _ == _           = False -- Default if checks fail
```
Because we defined `/=` in terms of `==` (And vice versa), we only need to 
overwrite one of the two operators (In this case, `==`)

You can make typeclasses depend on one another by using constraints
```haskell
class Eq a => Ord a where
    ...
```
When defining instances manually, the `a` must be a concrete type, although for
types like `Maybe x`, a type variable can be used

To see what the instances of a type class are, just type `:info YourTypeClass`
in `ghci`. This works for type constructors too (`:info Maybe`)

> ### Example: The `Truthy` type class
> In some weakly typed languages, almost any value can be interpreted as a 
> boolean. We want this kind of feature in haskell, so let's implement it 
> ourselves
```haskell
class Truthy a where
    isTruthy :: a -> Bool

instance Truthy Bool where
    isTruthy = id -- identity function

instance Truthy Int where
    isTruthy 0 = False
    isTruthy _ = True

instance Truthy [a] where
    isTruthy [] = False
    isTruthy _ = True

instance Truthy (Maybe a) where
    isTruthy Nothing = False
    isTruthy _ = True

instance Truthy (Either a b) where
    isTruthy (Left _) = False
    isTruthy _ = True

-- instance Truthy (Tree a) where
--     isTruthy Empty = False
--     isTruthy _ = True
```
### The `Functor` type class

The `Functor` type class corresponds to things that can be mapped over.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
`fmap` is the generic version of `map`, this last one only working for lists

```haskell
class Functor [] where
    fmap = map
```
In this case, `[]` is the constructor, not the empty list.
```haskell
instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Tree root left right) = Tree (f root) (fmap f left) (fmap f right)
```
You can also use partial application to define as instances of `Functor` other 
more interesting types
```haskell
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
```
```haskell
instance Functor (Data.Map.Map k) where
    ...
```
The type of a type is called a **kind**. If the type of a value is a little label
that we use to reason about the value, then the kinds are the labels we put over
types.

In `ghci`, kinds can be explored using `:k Type`
```haskell
Prelude> :k Int
Int :: *
```
The `*` indicates that the type is a concrete type, a type that doesn't take any
parameters
```haskell
Prelude> :k Maybe
Maybe :: * -> *
```
Knowing this, an instance of `Functor` must be of the kind `* -> *`

## Chapter 8 - Input and Output

To compile a haskell program (Named `FileName.hs`) use the following command on
the same directory
```bash
ghc --make FileName
```
Usign `FileName.hs` also works

```haskell
Prelude> :t putStrLn
putStrLn :: String -> IO ()
Prelude> :t putStrLn "Hello world"
putStrLn "Hello world" :: IO ()
```
This is read as: the type of `putStrLn` is a function that takes a `String` and 
returns a `IO` action that has a result type of `()` (The empty tuple, also 
known as _unit_)

An `IO` action is something that when performed will carry out an action with a
side effect and will also present some result (yields this result).

An `IO` action will be performed when we give it a name of `main` and then run 
our program

We can glue several `IO` actions into one using the `do` syntax.
```haskell
main = do
    putStrLn "Hello, What's your name?"
	name <- getLine
	putStrLn ("Hey " ++ name ++ ", you rock!")
```
`getLine` is a `IO` action that yields a string. The `<-` is an operator that
means "perform the action `<rhs>` and bind the result to `<lhs>`".
```haskell
getLine :: IO String
```
We could also bind with `<-` the answer of `putStrLn`, altough this would be a
`()`, but we can only do this with the first one. A `do` block doesn't allow 
binding the last action.

Inside a `do` block, we can use the `let` syntax to bind pure values to names
```haskell
reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main = do
       line <- getLine
	   if (null line)
	       then return ()
		   else do
		       putStrLn $ reverseWords line
			   main
```
The `return` function makes a `IO` action out of a pure value. It basically 
boxes the value.
```haskell
return :: Monal m => a -> m a
```
In our previous program, we use return to give an `IO` action that won't be 
actually performed. It's an instruction with no effect.

We could store the value in combination with `<-`.
```haskell
a <- return "hello"
```
We could say that `<-` and `return` are opposites

### Useful I/O functions
 * `putStr`
   ```haskell
   putStr :: String -> IO ()
   ```
   Takes a `String` and returns an action that will print it to the terminal, 
   without a newline `\n` at the end 

 * `putStrLn`
   ```haskell
   putStrLn :: String -> IO ()
   ```
   Takes a `String` and returns an action that will print it to the terminal, 
   with a newline `\n` at the end 

 * `putChar`
   ```haskell
   putChar :: Char -> IO ()
   ```
   Takes a `Char` and returns an action that will print it to the terminal

 * `print`
   ```haskell
   print :: Show a => a -> IO ()
   ```
   Takes an instance of `Show` and returns an action that will print it's 
   representation to the terminal, with a newline `\n` at the end 

 * `when` From `Control.Monad`
   ```haskell
   when :: Applicative f => Bool -> f () -> f ()
   ```
   Takes a `Bool` and a `IO` action, and if the first parameter is true, it 
   returns the provided `IO` action, else it returns an empty `IO`  
   _This function actually goes beyond IO_
   ```haskell
   -- EXAMPLE
   main = do
       input <- getLine
	   when (input == "SWORDFISH") $ do
	       putStrLn input
   ```

 * `sequence`
   ```haskell
   sequence :: (Traversable t, Monad m) => t (m a) -> m (t a)
   ```
   Takes a list of `IO` actions and returns a `IO` action that will perform
   those actions one after the other. The result that this action yields will be
   a list of the results of all the `IO` actions that were performed.
   ```haskell
   -- EXAMPLE
   sequence $ map print [1,2,3,4,5]
   ```
 * `mapM`
   ```haskell
   mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
   ```
   Takes a function that returns `IO` actions over the elements of a list and 
   the list, and returns an action of the sequence of said elements. It's 
   basically the composition of `sequence` and `map`  
   `mapM` is the same, but discarding the results later (Returns the equivalent
   of `return ()`)  
   _This function actually goes beyond IO_

 * `forever` From `Control.Monad`
    ```haskell
    forever :: Applicative f => f a -> f b
	```
	Takes an `IO` action and returns an `IO` action that repeats the `IO` action
	forever
	```haskell
	-- EXAMPLE
	main = forever $ do
	    putStr "Give me some input: "
		l <- getLine
		putStrLn $ map toUpper l
	```

 * `forM` 
   ```haskell
   forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
   ```
   Like `mapM` but with its arguments flipped. This can sometimes be useful.
   ```haskell
	-- EXAMPLE
	main = do
	    colors <- forM [1,2,3,4] (\a -> do
		    putStrLn $ "Which color do you associate with the number " 
			    ++ show a ++ "?"
		    color <- getLine
			return color)
		putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
		mapM putStrLn colors
   ```

To sum up, `IO` actions are types like any other in haskell, where the only 
particularity is that when they are inside the `main` function they are 
performed. These actions can also give you back information they got from the
real world.
