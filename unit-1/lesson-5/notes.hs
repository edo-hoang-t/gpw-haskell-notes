-- CLOSURES AND PARTIAL APPLICATION

-- 5.1. CLOSURES - CREATING FUNCTIONS WITH FUNCTIONS
ifEven f x =    if even x
                then f x
                else x

genIfEven f = (\x -> ifEven f x)

genIfXEven x = (\f -> ifEven f x)


-- 5.2. EXAMPLE: GENERATING URLS FOR AN API
getRequestURL host apiKey resource id = host ++ "/" ++ resource ++ "/" ++ id ++ "?token=" ++ apiKey

genHostRequestBuilder host = (\apiKey resource id -> getRequestURL host apiKey resource id) 

genApiRequestBuilder hostBuilder apiKey = (\resource id -> hostBuilder apiKey resource id)

genApiRequestBuilder_v2 hostBuilder apiKey resource = (\id -> hostBuilder apiKey resource id)

exampleUrlBuilder = getRequestURL "http://example.com" 
myExampleUrlBuilder = exampleUrlBuilder "1337hAsk3ll"
exampleBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "books"


-- 5.3. PUTTING IT ALL TOGETHER
-- addressLetterV2 location name = addressLetter name location 
flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x) 
-- addressLetterV2 = flipBinaryArgs addressLetter 
-- addressLetterNY = addressLetterV2 "ny"
subtract2 = flip (-) 2


-- SUMMARY
-- Q5.1
ifEvenInc = ifEven (\x -> x + 1)
ifEvenDouble = ifEven (\x -> x * 2)
ifEvenSquare = ifEven (\x -> x^2)

-- Q5.2
binaryPartialApplication f x1 = (\x2 -> f x1 x2)