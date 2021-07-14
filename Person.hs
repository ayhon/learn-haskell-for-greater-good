--                   Name   Lastnm Age Height Tlf    Icecream flavor
-- data Person = Person String String Int Float  String String
--     deriving (Show)

data Person = Person { firstName :: String,
                       lastName :: String,
                       age :: Int,
                       height :: Float,
                       phoneNumber :: String,
                       flavor :: String
                       } deriving (Show)
