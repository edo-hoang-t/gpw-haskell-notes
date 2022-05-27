-- CREATING TYPES WITH "AND" AND "OR"

-- 16.1. PRODUCT TYPES - COMBINING TYPES WITH "AND"
data AuthorName_v1 = AuthorName_v1 String String
data Book_v1 = Book_v1 AuthorName_v1 String String Int Double

data AuthorName_v2 = AuthorName_v2 {
    firstName_v2   ::  String,
    lastName_v2    :: String
}

data Book_v2 = Book_v2 {
    author_v2  ::  AuthorName_v2,
    isbn_v2    ::  String,
    title_v2   :: String,
    year_v2    :: Int,
    price_v2   :: Double
}


-- 16.2. SUM TYPES - COMBINING TYPES WITH "OR"
type FirstName  = String
type LastName   = String
type MiddleName = String

data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName
            | TwoInitialsWithLast Char Char LastName
            | FirstNameWithTwoInits FirstName Char Char -- Andrew W. K.

instance Show Name where
    show (Name fn ln) = fn ++ " " ++ ln
    show (NameWithMiddle fn mn ln) = fn ++ " " ++ mn ++ " " ++ ln
    show (TwoInitialsWithLast c1 c2 ln) = c1:'.':c2:'.':" " ++ ln
    show (FirstNameWithTwoInits fn c1 c2) = fn ++ c1:'.':c2:'.':" "

data Creator = AuthorCreator Author | ArtistCreator Artist

instance Show Creator where
    show (AuthorCreator (Author nm)) = show nm
    show (ArtistCreator (Person nm)) = show nm
    show (ArtistCreator (Band bd)) = bd

data Author = Author Name
data Artist = Person Name | Band String

-- e.g. H. P. Lovecraft
hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator
                (Author
                    (TwoInitialsWithLast 'H' 'P' "Lovecraft"))


-- 16.3. PUTTING TOGETHER YOUR BOOKSTORE
data Book = Book {
    author  ::  Creator,
    isbn    :: String,
    bookTitle   :: String,
    bookYear    :: Int,
    bookPrice   :: Double
}

data VinylRecord = VinylRecord {
    artist      :: Creator,
    recordTitle :: String,
    recordYear  :: Int,
    recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name        :: String,
    toyDescription :: String,
    toyPrice    :: Double
}

data Pamphlet = Pamphlet {
    pamphletTitle   :: String,
    pamphletDescription     :: String,
    contact         :: String
}

data StoreItem = BookItem Book 
                | RecordItem VinylRecord 
                | ToyItem CollectibleToy
                | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = "unknown"

-- Test
book1 = BookItem
            (Book {
                author = AuthorCreator (Author (Name "Clear" "James")),
                isbn = "A1B2C3D4",
                bookYear = 2019,
                bookTitle = "Atomic Habit",
                bookPrice = 9.01
            })


-- SUMMARY
-- Q16.1

-- Q16.2
type Radius = Double
type Width = Double
type Height = Double

data Shape = Circle Radius
            | Square Height
            | Rectangle Height Width
            deriving Show
        
perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r 
perimeter (Square h) = 4 * h 
perimeter (Rectangle h w) = 2 * (h + w)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Square h) = h * h 
area (Rectangle h w) = h * w
