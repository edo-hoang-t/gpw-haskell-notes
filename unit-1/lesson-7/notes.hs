-- RULES FOR RECURSION AND PATTERN MATCHING

-- 7.1. RECURSION


-- 7.2. RULES FOR RECURSION
-- RULE 1: Identify the end goal(s) 
-- RULE 2: Determine what happens when a goal is reached 
-- RULE 3: List all alternate possibilities 
-- RULE 4: Determine your “Rinse and Repeat” 
-- RULE 5: Ensure that each alterative moves you toward the goal 


-- 7.3. YOUR FIRST RECURSIVE FUNCTION: GREATEST COMMON DIVISOR
myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
            where remainder = a `mod` b

sayAmount_v1 n = case n of
                    1 -> "one"
                    2 -> "two"
                    n -> "a bunch"

sayAmount_v2 1 = "one" 
sayAmount_v2 2 = "two" 
sayAmount_v2 n = "a bunch"

isEmpty [] = True 
isEmpty _ = False 

myHead (x:xs) = x
myHead [] = error "No head for empty list"

-- myTail [] = []
myTail (_:xs) = xs
myTail [] = []


-- SUMMARY
-- Q7.1

-- Q7.2
myGCD_v2 a 0 = a
myGCD_v2 a b = myGCD_v2 b (a `mod` b)