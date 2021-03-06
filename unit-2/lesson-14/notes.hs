-- USING TYPE CLASSES
import Data.List

-- 14.1. A TYPE IN NEED OF CLASSES
data SixSidedDice = S1 | S2 | S3 | S4 | S5 | S6 deriving (Ord, Enum)


-- 14.2. IMPLEMENTING SHOW
instance Show SixSidedDice where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"


-- 14.3. TYPE CLASSES AND POLYMORPHISM
-- show :: SixSidedDice -> String 
-- show S1 = "one" 
-- show S2 = "two" 
-- show S3 = "three" 
-- show S4 = "four" 
-- show S5 = "five" 
-- show S6 = "six" 

-- data TwoSidedDie = One | Two 
-- show :: TwoSidedDie -> String 
-- show One = "one" 
-- show Two = "two" 


-- 14.4. DEFAULT IMPLEMENTATION AND MINIMUM COMPLETE DEFINITIONS
instance Eq SixSidedDice where
    (==) S6 S6 = True
    (==) S5 S5 = True
    (==) S4 S4 = True
    (==) S3 S3 = True
    (==) S2 S2 = True
    (==) S1 S1 = True
    (==) _ _ = False


-- 14.5. IMPLEMENTING ORD
-- instance Ord SixSidedDice where
--     compare S6 S6 = EQ
--     compare S6 _ = GT
--     compare _ S6 = LT
--     compare S5 S5 = EQ
--     compare S5 _ = GT
--     compare _ S5 = LT 
--     compare S4 S4 = EQ
--     compare S4 _ = GT
--     compare _ S4 = LT
--     compare S3 S3 = EQ
--     compare S3 _ = GT
--     compare _ S3 = LT 
--     compare S2 S2 = EQ
--     compare S2 _ = GT
--     compare _ S2 = LT
--     compare S1 S1 = EQ


-- 14.6. TO DERIVE OR NOT TO DERIVE?
-- instance Enum SixSidedDice where
--     toEnum 0 = S1
--     toEnum 1 = S2
--     toEnum 2 = S3
--     toEnum 3 = S4
--     toEnum 4 = S5
--     toEnum 5 = S6
--     toEnum _ = error "No such value"
--     fromEnum S1 = 0
--     fromEnum S2 = 1
--     fromEnum S3 = 2
--     fromEnum S4 = 3
--     fromEnum S5 = 4
--     fromEnum S6 = 5


-- 14.7. TYPE CLASSES FOR MORE-COMPLEX TYPES
data Name = Name (String, String) deriving (Show, Eq)
instance Ord Name where
    compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2) 

names :: [Name] 
names = [Name ("Emil","Cioran"), Name ("Eugene","Thacker"), Name ("Friedrich","Nietzsche")]

test_driver = do
    print (S1 == S1)
    print (S1 /= S2)
    print ([S1 .. S6])
    print ([S2,S4 .. S6])
    print [S1 .. ]
    print (sort names)


-- 14.8. TYPE CLASS ROADMAP


-- SUMMARY
-- Q14.1.
-- data Number = One | Two | Three deriving Enum
-- instance Eq Number where
--     (==) num1 num2 = (fromEnum num1) == (fromEnum num2)
-- instance Ord Number where
--     compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

-- Q14.2
data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Enum, Eq, Show)
class (Eq a, Enum a) => Die a where
    roll :: Int -> a
instance Die FiveSidedDie where
    roll n = toEnum (n `mod` 5) 