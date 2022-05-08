-- FIRST-CLASS FUNCTIONS
import Data.List

-- 4.1. FUNCTIONS AS ARGUMENTS
ifEven myFunction x =   if even x
                        then myFunction x
                        else x

inc n = n + 1
double n = n * 2

ifEvenInc n = ifEven inc n
ifEvenDouble n = ifEven double n
ifEvenSquare n = ifEven (\x -> x^2) n


names = [("Ian", "Curtis"), ("Bernard","Sumner"), ("Peter", "Hook"), ("Stephen","Morris")]

compareLastNames name1 name2 =  if lastName1 > lastName2
                                then GT
                                else    if lastName1 < lastName2
                                        then LT
                                        else    if firstName1 > firstName2
                                                then GT
                                                else    if firstName1 < firstName2
                                                        then LT
                                                        else EQ
                                where   lastName1 = snd name1
                                        lastName2 = snd name2
                                        firstName1 = fst name1
                                        firstName2 = fst name2

sortByLastName = sortBy compareLastNames names

-- 4.2. RETURNING FUNCTIONS
sfOffice name = if lastName < "L" 
                then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111" 
                else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
                where   lastName = snd name 
                        nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
                where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name =   nameText ++ " - PO Box 456 - Reno, NV 89523"
                    where nameText = snd name

dcOffice name = nameText ++ " PO Box 1337 - Washington DC, 20001"
                where nameText = (fst name) ++ " " ++ (snd name) ++ ", Esq." 

getLocationFunction location =  case location of
                                    "ny" -> nyOffice 
                                    "sf" -> sfOffice 
                                    "reno" -> renoOffice
                                    "dc" -> dcOffice
                                    _ -> (\name -> (fst name) ++ " " ++ (snd name))

addressLetter name location =   locationFunction name
                                where locationFunction = getLocationFunction location

-- SUMMARY
-- Q4.1
compareLastNames_v2 name1 name2 =   if result == EQ
                                    then compare (fst name1) (fst name2)
                                    else result
                                    where result = compare (snd name1) (snd name2)