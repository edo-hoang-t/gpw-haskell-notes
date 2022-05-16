-- TYPE CLASSES
import Data.Word

-- 13.1. FURTHUR EXPLORING TYPES
-- :t ["cat", "dog"]


-- 13.2. TYPE CLASSES
-- :info Num 


-- 13.3. THE BENEFITS OF TYPE CLASSES
addThenDouble :: Num a => a -> a -> a
addThenDouble x y = (x + y) * 2


-- 13.4. DEFINING A TYPE CLASS
class Describable a where
    describe :: a -> String


-- 13.5. COMMON TYPE CLASSES


-- 13.6. THE Ord AND Eq TYPE CLASSES
-- GHCi> True 
-- True 
-- GHCi> False 
-- False 
-- GHCi> Chocolate 
-- <interactive>:404:1: 
-- No instance for (Show Icecream) arising from a use of ‘print’ 
-- In a stmt of an interactive GHCi command: print it 


-- 13.7. DERIVING TYPE CLASSES
data IceCream = Chocolate | Vanilla | Cheese deriving (Show, Eq, Ord)
main = do
    print Chocolate
    print Vanilla
    print (Chocolate == Vanilla)
    print (Chocolate /= Vanilla)
    print (Chocolate < Vanilla)
    print (Vanilla > Cheese)


-- SUMMARY
-- Q13.1
main2 = do
    print (minBound :: Int)
    print (maxBound :: Int)
    print (minBound :: Word)
    print (maxBound :: Word)

-- Q13.2
inc :: Int -> Int 
inc x = x + 1

main3 = do
    print (inc maxBound :: Int)
    -- print (succ maxBound :: Int)

-- Q13.3
cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a 
cycleSucc n =   if n == maxBound
                then minBound
                else succ n
-- Test Drive
data TestCycle = T1 | T2 | T3 | T4 deriving (Bounded, Enum, Eq, Show)

main4 = do
    print (cycleSucc T1)
    print (cycleSucc T2)
    print (cycleSucc T3)
    print (cycleSucc T4)