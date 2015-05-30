multiply x y z = double x * double y * double z
double x = x * 2
concatStrings x y = x ++ y ++ "!"

-- Just sample lucky number function using pattern matching
luckyNumber :: Int -> String
luckyNumber 7 = "Hey that's your lucky number !"
luckyNumber x = "Hard Luck Buddy. Try Again !"

-- A sample sum function to add the elements in a list using recursion & pattern matching
sum' :: [Int] -> Int
sum' [] = error "You've provided an empty list"
sum' [x] = x
sum' (x:xs) = x + sum'(xs)

-- An attempt to implement the Haskell "take" function using recursion & pattern matching
take' :: Int -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) (xs)

-- An attempt to implement the Haskell "reverse" function using recursion & pattern matching
reverse' :: (Num a) => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' (xs) ++ [x]

-- An attempt to implement a function which finds the maximum value in list using recursion & pattern matching
maximum' :: (Ord a)=> [a] -> a
maximum' [] = error "No maximum for an empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- An attempt to implement the Haskell "replicate" function using recursion & pattern matching
replicate' :: Int -> a -> [a]
replicate' n x
		| n <= 0 = []
replicate' n x = [x] ++ replicate' (n-1) x

-- An attempt to implement the Haskell "zip" function using recursion & pattern matching
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

-- An attempt to implement the Haskell "elem" function using recursion & pattern matching
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) = if (n == x) then True else elem' n (xs)

--An attempt to implement the "Quick Sort Algorithm" using recursion and pattern matching in Haskell language
quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
                 let leftSide = [ a | a <- xs, a <= x ]
                     rightSide = [ a | a <- xs, a > x ] 
                     in quickSort leftSide ++ [x] ++ quickSort rightSide
