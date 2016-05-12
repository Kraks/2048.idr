-- 2048 Game
-- Guannan Wei

import Data.List
import Effects
import Effect.Exception
import Effect.StdIO
import Effect.Random
import Effect.System

data Direction = Up | Down | Left | Right
data Action = Exit | Merge Direction | Invalid

Line : Type
Line = List Nat

Grid : Type
Grid = List Line

range : Int -> Int -> List Int
range x y = if x < y then x::(range (x+1) y) else []

rangeFromZero : Int -> List Int
rangeFromZero = range 0

ormap : (a -> Bool) -> List a -> Bool
ormap f [] = False
ormap f (x::xs) = (f x) || (ormap f xs)

andmap : (a -> Bool) -> List a -> Bool
andmap f [] = True
andmap f (x::xs) = (f x) && (andmap f xs)

buildList : Nat -> (Nat -> a) -> List a
buildList Z f = []
buildList (S k) f = (f (S k))::(buildList k f)

emptyLine : Nat -> Line
emptyLine n = buildList n (\i => Z)

emptyGrid : Nat -> Grid
emptyGrid n = buildList n (\i => emptyLine n)

flatten : List (List a) -> List a
flatten [] = []
flatten (x::xs) = x ++ (flatten xs)

partition : List a -> Nat -> List (List a)
partition [] _ = []
partition xs step = if (List.length xs) <= step
                       then xs::[]
                       else (List.take step xs)::(partition (List.drop step xs) step)

--(.) : (b -> c) -> (a -> b) -> a -> c
--(.) f g = \x => f (g x)

--------------------

-- merge from right to left
-- [2, 0, 2, 0] -> [4, 0, 0, 0]
mergeLine : Line -> Line
mergeLine []          = []
mergeLine (x::[])     = [x]
mergeLine (Z::xs)     = (mergeLine xs) ++ [Z]
mergeLine (x::Z::xs)  = (mergeLine (x::xs)) ++ [Z]
mergeLine (x::x'::xs) = if x == x' then ((x+x')::xs) ++ [Z]
                                   else x::(mergeLine (x'::xs))

mergeLineLeft : Line -> Line
mergeLineLeft = mergeLine

mergeLineRight : Line -> Line
mergeLineRight = reverse . mergeLineLeft . reverse

mergeLeft : Grid -> Grid
mergeLeft = map mergeLineLeft

mergeRight : Grid -> Grid
mergeRight = map mergeLineRight

mergeUp : Grid -> Grid
mergeUp = transpose . mergeLeft . transpose

mergeDown : Grid -> Grid
mergeDown = transpose . mergeRight . transpose

move : Direction -> Grid -> Grid
move Left  = mergeLeft
move Right = mergeRight
move Up    = mergeUp
move Down  = mergeDown

--------------------

winNumber : Nat
winNumber = 2048

isWin : Grid -> Bool
isWin = ormap (ormap (== winNumber))

--------------------

isLineFail : Line -> Bool
isLineFail xs = 
  let (S k) = List.length xs
      zipped = List.zip (List.take k xs) (List.drop 1 xs)
  in andmap (\p => not ((fst p) == (snd p) || (0 == (fst p)) || (0 == (snd p)))) zipped

isFail : Grid -> Bool
isFail g = (andmap isLineFail g) && (andmap isLineFail (transpose g))

--------------------

listSet : List a -> Nat -> a -> List a
listSet [] _ _ = []
listSet (x::xs) Z ele = ele::xs
listSet (x::xs) (S k) ele = x::(listSet xs k ele)

showLine : Line -> String
showLine [] = ""
showLine (x::xs) = (show x) ++ " " ++ (showLine xs)

showGrid : Grid -> String
showGrid [] = ""
showGrid (l::ls) = (showLine l) ++ "\n" ++ (showGrid ls)

strToAction : String -> Action
strToAction "w" = Merge Up
strToAction "a" = Merge Left
strToAction "s" = Merge Down
strToAction "d" = Merge Right
strToAction "q" = Exit
strToAction  _  = Invalid

randomNumber : Eff Nat [RND, SYSTEM]
randomNumber = do srand !time
                  x <- rndInt 0 10000
                  return (fromInteger x)

addRandom : Grid -> Eff Grid [RND, EXCEPTION String]
addRandom g = case !(rndSelect seeds) of
    Nothing => raise "Game Over"
    Just idx => return (partition (listSet flattenedGrid idx 2) 4)
  where 
    seeds : List Nat
    seeds = [2, 2, 2, 4]
    flattenedGrid : List Nat
    flattenedGrid = flatten g
    zeroIndices : List Nat
    zeroIndices = findIndices (== 0) flattenedGrid

loop : Grid -> Eff (Grid) [RND, EXCEPTION String, SYSTEM, STDIO]
loop g = 
  do putStrLn (showGrid g)
     c <- getStr
     doAction (strToAction c)
   where doAction : Action -> Eff (Grid) [RND, EXCEPTION String, SYSTEM, STDIO]
         doAction (Merge d) = 
           let g' = move d g in
               if g == g' then loop g'
               else if isWin g' then
                 do putStrLn (showGrid g')
                    raise "You Win!"
               else if isFail g' then 
                 do putStrLn (showGrid g')
                    raise "You Fail!"
               else do g'' <- addRandom g' 
                       loop g''
         doAction Exit = raise "Exit."
         doAction Invalid = loop g

startGame : Eff () [RND, SYSTEM, STDIO, EXCEPTION String]
startGame = do srand !time
               initGrid <- addRandom (emptyGrid 4)
               loop initGrid
               return ()

main : IO ()
main = run startGame

-- test
g : Grid
g = [[2, 2, 4, 4],
     [0, 0, 2, 2],
     [0, 2, 2, 2],
     [4, 4, 4, 4]]
