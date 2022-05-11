-- HIGHER-ORDER FUNCTIONS

-- 9.1. USING MAP
map reverse ["dog","cat", "moose"] 
-- ["god","tac","esoom"]
map head ["dog","cat", "moose"] 
-- "dcm"
map (take 4) ["pumpkin","pie","peanut butter"] 
-- ["pump","pie","pean"] 


-- 9.2. ABSTRACTING AWAY RECURSION WITH MAP
map ("a "++) ["train","plane","boat"] 
-- ["a train","a plane","a boat"] 
map (^2) [1,2,3] 
-- [1,4,9]

myMap f [] = []
myMap f (x:xs) = (f x):(myMap f xs)


-- 9.3. FILTERING A LIST
filter even [1,2,3,4] 
-- [2,4]
filter (\(x:xs) -> x == 'a') ["apple","banana","avocado"] 
-- ["apple","avocado"]

myFilter test [] = []
myFilter test (x:xs) =  if test x
                        then x:(myFilter test xs)
                        else myFilter test xs


-- 9.4. FOLDING A LIST
foldl (+) 0 [1,2,3,4] 
-- 10 
-- Combining 'map' and 'foldl'
sumOfSquares xs = foldl (+) 0 (map (^2) xs)

rcons x y = y:x 
myReverse xs = foldl rcons [] xs 

myFoldl f init [] = init 
myFoldl f init (x:xs) = myFoldl f newInit xs
                        where newInit = f init x 

myFoldr f init [] = init 
myFoldr f init (x:xs) = f x rightResult
                        where rightResult = myFoldr f init xs 

foldl (+) 0 [1,2,3,4] 
-- 10 
foldr (+) 0 [1,2,3,4] 
-- 10 
foldl (-) 0 [1,2,3,4] 
-- -10 
foldr (-) 0 [1,2,3,4] 
-- -2 


-- SUMMARY
-- Q9.1
myElem el xs = length (filter (== el) xs) >= 1

-- Q9.2
import Data.Char
isPalindrome text = processedText == reverse processedText
                    where   noSpaces = filter (/= ' ') text
                            processedText = map toLower noSpaces

-- Q9.3 - use lazy evaluation
harmonic n =    sum (take n seriesValues)
                where   seriesPairs = zip (cycle [1]) [1 .. ]
                        seriesValues = map (\pair -> (fst pair) / (snd pair)) seriesPairs