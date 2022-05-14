-- TYPE BASICS

-- 11.1. TYPES IN HASKELL
x :: Int
x = 2

main1 = do
    print (x * 2000)
    print (x^2000)

-- Integer type
y :: Integer
y = 2

main2 = do
    print (y * 2000)
    print (y^2000)

-- Common types: Char, Double, and Bool
letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 0.375

isFun :: Bool
isFun = True

-- List types
values :: [Int]
values = [1, 2, 3]

testScores :: [Double]
testScores = [0.1, 2.4]

letters :: [Char]
letters = ['a', 'b', 'c']

main3 = do
    print (letters == "abc")

anotherPet :: String 
anotherPet = "dog" 

-- Tuple types
ageAndHeight :: (Int, Int)
ageAndHeight = (23, 74)


-- 11.2. FUNCTION TYPES
double :: Int -> Int
double n = n * 2

half :: Int -> Double
-- half n = n / 2
half n = (fromIntegral n) / 2
-- 5 / 2

main4 = do
    print (show 6)
    print (show 'c')
    print (show 6.0)

z = read "6" 
q = z / 2

anotherNumber :: Int
anotherNumber = read "6"

number6 = read "6" :: Double

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress number street town = (number, street, town)

main5 = do
    print (makeAddress 123 "Happy St" "Haskell Town") 

ifEven :: (Int -> Double) -> Int -> Double
ifEven f n =    if even n
                then f n 
                else fromIntegral n

ifEvenDivideHalf n = ifEven half n


-- 11.3. TYPE VARIABLES
simple :: a -> a 
simple x = x

makeTriple :: a -> b -> c -> (a,b,c) 
makeTriple x y z = (x, y, z)

-- map :: (a -> b) -> [a] -> [b]
-- map show [1,2,3,4]


-- SUMMARY
-- Q11.1
-- filter (\x -> x > 3) xs
-- filter :: (a -> Bool) -> [a] -> [a]

-- Q11.2
myTail :: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

-- Q11.3
myFoldl :: (a -> b -> a) -> a -> [b] -> a