-- LAMBDA FUNCTIONS AND LEXICAL SCOPE

-- 3.1. LAMBDA FUNCTIONS
main = do
    print ((\x -> x) [1, 2, 3])

-- 3.2. WRITING YOUR OWN WHERE CLAUSE
sumSquareOrSquareSum_v1 x y =  if sumSquare > squareSum 
                            then sumSquare 
                            else squareSum
                            where   sumSquare = x^2 + y^2 
                                    squareSum = (x+y)^2

sumSquareOrSquareSum_v2 x y =   if (x^2 + y^2) > ((x + y)^2)
                                then (x^2 + y^2)
                                else (x + y)^2

body sumSquare squareSum =   if sumSquare > squareSum
                            then sumSquare
                            else squareSum

sumSquareOrSquareSum_v3 x y = body (x^2 + y^2) ((x + y)^2)

sumSquareOrSquareSum_v4 x y = (\sumSquare squareSum ->  if sumSquare > squareSum
                                                    then sumSquare
                                                    else squareSum) (x^2 + y^2) ((x + y)^2)

-- 3.3. FROM LABMDA TO LET: MAKING YOUR OWN VARIABLE VARIABLES
sumSquareOrSquareSum_v5 x y =   let sumSquare = (x^2 + y^2)
                                    squareSum = (x + y)^2
                                in
                                    if sumSquare > squareSum
                                    then sumSquare
                                    else squareSum

overwrite_v1 x =    let x = 2
                    in
                        let x = 3
                        in
                            let x = 4
                            in
                                x

overwrite_v2 x =    (\x -> 
                        (\x -> 
                            (\x -> x) 4
                        ) 3
                    ) 2

-- 3.4. PRACTICAL LAMBDA FUNCTIONS AND LEXICAL SCOPE
x = 4

add1 y = y + x

add2 y = (\x -> y + x) 3

add3 y =    (\y -> 
                (\x -> y + x) 1
            ) 2

-- SUMMARY
-- Q3.2
counter_v1 x =  let x = x + 1
                in
                    let x = x + 1
                    in
                        x

counter_v2 x =  (\x -> 
                    (\x -> x) (x + 1)
                ) (x + 1)