-- CAPSTONE: FUNCTIONAL OBJECT-ORIENTED PROGRAMMING WITH ROBOTS!

-- 10.1. AN OBJECT WITH ONE PROPERTY: A CUP OF COFFEE
-- cup fluidOz = \_ -> fluidOz

cup fluidOz = \message -> message fluidOz
coffeeCup = cup 12

getOz aCup = aCup (\flOz -> flOz) -- GETTER
-- getOz coffeeCup

drink aCup ozDrank =    if ozDiff >= 0
                        then cup ozDiff
                        else cup 0
                        where   flOz = getOz aCup
                                ozDiff = flOz - ozDrank
                            
afterASip = drink coffeeCup 1
-- getOz afterASip
afterBigGulp = drink coffeeCup 20
-- getOz afterBigGulp

isEmpty aCup = getOz aCup == 0
-- isEmpty afterBigGulp

afterManySips = foldl drink coffeeCup [1,2,3]
-- getOz afterManySips


-- 10.2. A MORE COMPLEX OBJECT: LET'S BUILD FIGHTING ROBOTS!
robot (name,attack,hp) = \message -> message (name,attack,hp) -- constructor

killerRobot = robot ("Kill3r",25,200)

-- GETTER's helpers
name (n,_,_) = n 
attack (_,a,_) = a 
hp (_,_,hp) = hp

-- GETTERs
getName aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP aRobot = aRobot hp

-- getAttack killerRobot
-- getHP killerRobot

-- SETTERs
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setAttack aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))

printRobot aRobot = aRobot (\(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:"++ (show h)) 
-- show: convert variables a and h to string to concatenate

damage aRobot attackDamage = aRobot (\(n,a,h) -> robot (n,a,h-attackDamage))
afterHit = damage killerRobot 90
-- getHP afterHit

fight aRobot defender = damage defender attack
                        where attack =  if getHP aRobot > 10
                                        then getAttack aRobot
                                        else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

gentleGiantRound1 = fight killerRobot gentleGiant
killerRobotRound1 = fight gentleGiant killerRobot
gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2


-- 10.3. WHY STATELESS PROGRAMMING MATTERS
fastRobot = robot ("speedy", 15, 40) 
slowRobot = robot ("slowpoke",20,30)

fastRobotRound1 = fight slowRobotRound1 fastRobot 
fastRobotRound2 = fight slowRobotRound2 fastRobotRound1 
fastRobotRound3 = fight slowRobotRound3 fastRobotRound2 
slowRobotRound1 = fight fastRobot slowRobot 
slowRobotRound2 = fight fastRobotRound1 slowRobotRound1 
slowRobotRound3 = fight fastRobotRound2 slowRobotRound2 

