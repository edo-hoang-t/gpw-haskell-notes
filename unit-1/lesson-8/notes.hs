-- WRITING RECURSION FUNCTIONS

-- 8.1. REVIEW: RULES OF RECURSION


-- 8.2. RECURSIONS ON LISTS
-- 8.2.1. Implement length
myLength [] = 0
myLength xs = 1 + myLength (tail xs)

myLength_v2 [] = 0
myLength_v2 (x:xs) = 1 + myLength_v2 xs

-- 8.2.2. Implement take
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) =   x:rest
                    where rest = myTake (n - 1) xs

-- 8.2.3. Implement cycle
finiteCycle (first:rest) = (first:rest) ++ [first]
myCycle (first:rest) = first:myCycle (rest ++ [first])


-- 8.3. PATHOLOGICAL RECURSION: ACKERMAN FUNCTION AND THE COLLATZ CONJECTURE
-- 8.3.1. The Ackermann function
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))
-- :set +s
-- ackermann 3 3
-- ackermann 3 8
-- ackermann 3 9

-- 8.3.2. THE Collatz conjecture
collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n*3 + 1)


-- SUMMARY
-- Q8.1
myReverse [] = [] 
myReverse (x:xs) = (myReverse xs) ++ [x] 

-- Q8.2 - Memoization
fastFib _ _ 0 = 0 
fastFib _ _ 1 = 1 
fastFib _ _ 2 = 1 
fastFib x y 3 = x + y 
fastFib x y c = fastFib (x + y) x (c - 1)
fib n = fastFib 1 1 n