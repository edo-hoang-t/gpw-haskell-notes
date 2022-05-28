-- DESIGN BY COMPOSITION - SEMIGROUPS AND MONOIDS
import Data.List
import Data.Semigroup

-- 17.1. INTRO TO COMPOSABILITY - COMBINING FUNCTIONS
myLast :: [a] -> a 
myLast xs = (head . reverse) xs

myMin :: Ord a => [a] -> a 
myMin = head . sort

myMax :: Ord a => [a] -> a 
myMax = myLast . sort

myAll :: (a -> Bool) -> [a] -> Bool
myAll testFunc = (foldr (&&) True) . (map testFunc)

test1 = do
    print (myAll even [1,2,3])
    

-- 17.2. COMBINING LIKE TYPES: SEMIGROUPS
-- 17.2.1. The Color Semigroup
data Color = Red | Green | Blue | Yellow | Purple | Orange | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Yellow = Orange
    (<>) Yellow Red = Orange
    (<>) Blue Yellow = Green
    (<>) Yellow Blue = Green
    (<>) Red Blue = Purple
    (<>) Blue Red = Orange
    (<>) a b    | a == b = a 
                | all (\a -> elem a [Red,Yellow,Orange]) [a,b] = Orange -- add associativity
                | all (\a -> elem a [Blue,Yellow,Green]) [a,b] = Green 
                | all (\a -> elem a [Red,Green,Purple]) [a,b] = Purple
                | otherwise = Brown

test2 = do
    print ((Green <> Blue) <> Yellow)
    print (Green <> (Blue <> Yellow))


-- 17.3. COMPOSING WITH IDENTITY: MONOIDS
-- [1,2,3] <> []
-- [1,2,3] `mappend` mempty
-- 17.3.1. mconcat: Combining multiple Monoids at once
-- mconcat ["does"," this"," make"," sense?"] 

-- 17.3.2. Monoid Laws

-- 17.3.3. Practical Monoids - building probability tables





















main = do

    