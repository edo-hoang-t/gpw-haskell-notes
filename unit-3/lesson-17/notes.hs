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
data Color = Red | Green | Blue | Yellow 
            | Purple | Orange | Brown | Clear deriving (Show, Eq)

instance Semigroup Color where
    (<>) Clear any = any
    (<>) any Clear = any
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
type Events = [String] 
type Probs = [Double] 

data PTable = PTable Events Probs 
createPTable :: Events -> Probs -> PTable 
createPTable events probs = PTable events normalizedProbs
                            where totalProbs = sum probs 
                                  normalizedProbs = map (\x -> x/totalProbs) probs 

showPair :: String -> Double -> String 
showPair event prob = mconcat [event,"|", show prob,"\n"] 

-- zipWith (+) [1,2,3] [4,5,6] 
instance Show PTable where
    show (PTable events probs) = mconcat pairs
                                where pairs = zipWith showPair events probs

-- createPTable ["heads","tails"] [0.5,0.5] 

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c] 
cartCombine func l1 l2 = zipWith func newL1 cycledL2
                        where nToAdd = length l2 
                              repeatedL1 = map (take nToAdd . repeat) l1 
                              newL1 = mconcat repeatedL1 
                              cycledL2 = cycle l2

combineEvents :: Events -> Events -> Events  
combineEvents e1 e2 = cartCombine combiner e1 e2 
                      where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs 
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2   
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
                                        where newEvents = combineEvents e1 e2 
                                              newProbs = combineProbs p1 p2 

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>) 

coin :: PTable 
coin = createPTable ["heads","tails"] [0.5,0.5] 

spinner :: PTable 
spinner = createPTable ["red","blue","green"] [0.1,0.2,0.7]

-- coin <> spinner
-- mconcat [coin,coin,coin]


-- SUMMARY
-- Q17.1
instance Monoid Color where
    mempty = Clear
    mappend col1 col2 = col1 <> col2

-- Q17.2
data Events_v2 = Events_v2 [String]
data Probs_v2 = Probs_v2 [Double]

combineEvents_v2 :: Events_v2 -> Events_v2 -> Events_v2  
combineEvents_v2 (Events_v2 e1) (Events_v2 e2) = Events_v2 (cartCombine combiner e1 e2)
                      where combiner = (\x y -> mconcat [x,"-",y])

instance Semigroup Events_v2 where
    (<>) = combineEvents_v2

instance Monoid Events_v2 where
    mempty = Events_v2 []
    mappend = (<>)

combineProbs_v2 :: Probs_v2 -> Probs_v2 -> Probs_v2 
combineProbs_v2 (Probs_v2 p1) (Probs_v2 p2) = Probs_v2 (cartCombine (*) p1 p2)

instance Semigroup Probs_v2 where
    (<>) = combineProbs_v2

instance Monoid Probs_v2 where
    mappend = (<>)
    mempty = Probs_v2 [] 