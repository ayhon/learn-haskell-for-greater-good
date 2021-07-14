data Vector a = Vector a a a 
    deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector a b c) (Vector x y z) 
    =  Vector (a+x) (b+y) (c+z)

dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector a b c) (Vector x y z)
    = a*x + b*y + c*z

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector x y z) k 
    = Vector (x*k) (y*k) (z*k)



