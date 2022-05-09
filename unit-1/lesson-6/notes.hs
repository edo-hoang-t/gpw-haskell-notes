-- LISTS

-- 6.1. THE ANATOMY OF A LIST
head [1,2,3]
-- 1
head [[1,2],[3,4],[5,6]] 
-- [1,2]
tail [1,2,3]
-- [2,3]
tail [3]
-- []
tail []
-- *** Exception: Prelude.tail: empty list
head []
-- *** Exception: Prelude.head: empty list
1:[]
-- [1]
1:2:3:4:[]
-- [1,2,3,4]
(1,2):(3,4):(5,6):[] 
-- [(1,2),(3,4),(5,6)] 
1:[2,3,4] 
-- [1,2,3,4]
['h','e','l','l','o']
-- "hello"
'h':'e':'l':'l':'o':[] 
-- "hello"
'h':"ello" 
-- "hello"
"h" ++ "ello"
-- "hello"
[1] ++ [2,3,4] 
-- [1,2,3,4]


-- 6.2. LISTS AND LAZY EVALUATION
[1 .. 10]
-- [1,2,3,4,5,6,7,8,9,10] 
[1,3 .. 10]
-- [1,3,5,7,9]
[1, 1.5 .. 5] 
-- [1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0]
[1,0 .. -10]
-- [1,0,-1,-2,-3,-4,-5,-6,-7,-8,-9,-10]
[1 .. ] 
-- [1,2,3,4,5,6,7,8,9,10,11,12 ..
simple x = x 
longList = [1 .. ] 
stillLongList = simple longList 

backwardsInfinity = reverse [1..]


-- 6.3. COMMON FUNCTIONS ON LISTS
[1,2,3] !! 0 
-- 1
"puppies" !! 4 
-- 'i'
[1..10] !! 11 
-- *** Exception: Prelude.(!!): index too large
(!!) [1,2,3] 0 
-- 1
paExample1 = (!!) "dog" 
paExample1 2 
-- 'g'
paExample2 = ("dog" !!) 
paExample2 2 
-- 'g'
paExample3 = (!! 2) 
paExample3 "dog" 
-- 'g'
length [1..20] 
-- 20
length [(10,20),(1,2),(15,16)] 
-- 3
length "quicksand" 
-- 9
reverse [1,2,3]
-- [3,2,1]
reverse "cheese" 
-- "eseehc"
isPalindrome word = word == reverse word 
isPalindrome "cheese" 
-- False
isPalindrome "racecar" 
-- True
isPalindrome [1,2,1]
-- True
elem 13 [0,13 .. 100] 
-- True
elem 'p' "cheese" 
-- False
respond phrase =    if '!' `elem` phrase
                    then "wow!"
                    else "uh.. okay"
respond "hello" 
-- "uh.. okay" 
respond "hello!"
-- "wow!"
take 5 [2,4..100] 
-- [2,4,6,8,10] 
take 3 "wonderful" 
-- "won" 
take 1000000 [1] 
-- [1]
takeLast n aList = reverse (take n (reverse aList)) 
takeLast 10 [1..100] 
-- [91,92,93,94,95,96,97,98,99,100] 
drop 2 [1,2,3,4,5] 
-- [3,4,5] 
drop 5 "very awesome" 
-- "awesome" 
zip [1,2,3] [2,4,6] 
-- [(1,2),(2,4),(3,6)] 
zip "dog" "rabbit" 
-- [('d','r'),('o','a'),('g','b')] 
zip ['a' .. 'f'] [1 .. ] 
-- [('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)] 
ones n = take n (cycle [1]) 
ones 2
-- [1,1] 
assignToGroups n aList =    zip groups aList
                            where groups = cycle [1..n] 
assignToGroups 3 ["file1.txt","file2.txt","file3.txt","file4.txt","file5.txt","file6.txt","file7.txt","file8.txt"]
-- [(1,"file1.txt"),(2,"file2.txt"),(3,"file3.txt"),(1,"file4.txt"),(2,"file5.txt"),(3,"file6.txt"),(1,"file7.txt"),(2,"file8.txt")]


-- SUMMARY
-- Q6.1
repeat x = cycle [x]
-- Q6.2
subseq start_pos end_pos ls = drop start_pos (take end_pos ls)  
-- Q6.3
inFirstHalf el ls = el `elem` firstHalf 
                    where   midpoint = length ls `div` 2 
                            firstHalf = take midpoint ls