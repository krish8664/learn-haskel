module DataTypes where

------ Map implementation
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x:(myMap f xs)

------ Filter implementation
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter f (x:xs) = if f x then x:(myFilter f xs) else (myFilter f xs)

------ Fold implementation
myFold :: (r -> a -> r) -> r -> [a] -> r
myFold _ r []     = r
myFold f r (x:xs) = myFold f (f r x) xs
