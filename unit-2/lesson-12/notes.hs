-- CREATING YOUR OWN TYPES

-- 12.1. USING TYPE SYNONYMS
type FirstName = String
type LastName = String
type Age = Int
type Height = Int

patientInfo :: FirstName -> LastName -> Age -> Height -> String
patientInfo fname lname age height =    name ++ " " ++ ageHeight 
                                        where name = lname ++ ", " ++ fname
                                              ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

type PatientName = (String, String)
firstName :: PatientName -> String
firstName patient = fst patient
lastName :: PatientName -> String
lastName patient = snd patient

testPatient = ("Strange", "Stephen")
patientInfo_v2 :: PatientName -> Age -> Height -> String
patientInfo_v2 patientName age height = name ++ " " ++ ageHeight
                                        where name = (fst patientName) ++ ", " ++ (snd patientName)
                                              ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

patientInfo_v3 :: PatientName -> Age -> Height -> String
patientInfo_v3 (fname,lname) age height = name ++ " " ++ ageHeight
                                            where name = lname ++ ", " ++ fname
                                                  ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)" 


-- 12.2. CREATING NEW TYPES
data Sex = Male | Female
sexInitial :: Sex -> Char
sexInitial Male = 'M'
sexInitial Female = 'F'

data RhType = Pos | Neg
data ABOType = A | B | AB | O 
data BloodType = BloodType ABOType RhType

patient1BT :: BloodType
patient1BT = BloodType A Pos
patient2BT :: BloodType
patient2BT = BloodType O Neg
patient3BT :: BloodType
patient3BT = BloodType AB Pos

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String 
showABO A = "A" 
showABO B = "B" 
showABO AB = "AB" 
showABO O = "O" 
showBloodType :: BloodType -> String 
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

main1 = do
    print (canDonateTo patient1BT patient2BT) 
    print (canDonateTo patient2BT patient1BT)
    print (canDonateTo patient2BT patient3BT)
    print (canDonateTo patient1BT patient3BT)
    print (canDonateTo patient3BT patient1BT)

type MiddleName = String
data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l 
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

main2 = do
    print (showName name1)
    print (showName name2)


-- 12.3. USING RECORD SYNTAX
data Patient = Patient {
    name:: Name,
    sex :: Sex,
    age :: Int,
    height :: Int,
    weight :: Int,
    bloodType :: BloodType
}

jackieSmith :: Patient
jackieSmith = Patient {
    name = Name "Jackie" "Smith",
    age = 43,
    sex = Female,
    height = 62,
    weight = 115,
    bloodType = BloodType O Neg
}

main3 = do
    print (height jackieSmith)
    print (showBloodType (bloodType jackieSmith))
    print (showName (name jackieSmith))

jackieSmithUpdated = jackieSmith {
    age = 44
}


-- SUMMARY
-- Q12.1
canDonateTo_v2 :: Patient -> Patient -> Bool
canDonateTo_v2 pat1 pat2 = canDonateTo (bloodType pat1) (bloodType pat2)

-- Q12.2
showNameComma :: Name -> String
showNameComma (Name f l) = f ++ ", " ++ l
showSex :: Sex -> String
showSex Male = "Male"
showSex Female = "Female"

patientSummary :: Patient -> String
patientSummary pat =    "**************\n"
                    ++  "Patient Name: " ++ (showNameComma (name pat)) ++ "\n"
                    ++  "Sex: " ++ showSex (sex pat) ++ "\n"
                    ++  "Age: " ++ show (age pat) ++ "\n"
                    ++  "Height: " ++ show (height pat) ++ " in.\n"
                    ++  "Weight: " ++ show (weight pat) ++ " lbs.\n"
                    ++  "Blood Type: " ++ showBloodType (bloodType pat) ++ "\n"
                    ++  "**************\n"

main4 = do
    putStr (patientSummary jackieSmith)