-- 2048 Game
-- Guannan Wei

import Data.List
import Data.Vect
import Effects
import Effect.Exception
import Effect.StdIO
import Effect.Random
import Effect.System

data Direction = Up | Down | Left | Right
data Action = Exit | Merge Direction | Invalid

orvmap : (a -> Bool) -> Vect n a -> Bool
orvmap f [] = False
orvmap f (x::xs) = (f x) || (orvmap f xs)

andvmap : (a -> Bool) -> Vect n a -> Bool
andvmap f [] = True
andvmap f (x::xs) = (f x) && (andvmap f xs)

flatten : {n : Nat} -> {m: Nat} -> Vect n (Vect m a) -> Vect (n*m) a
flatten [] = []
flatten (x::xs) = x ++ (flatten xs)

partition : {n: Nat} -> {m: Nat} -> Vect (n*m) a -> Vect n (Vect m a)
partition {n=Z} {m=q} xs = []
partition {n=(S k)} {m=(S l)} xs = let rest = (Main.partition {n=k} {m=(S l)} (Vect.drop (S l) xs)) 
                                   in (Vect.take (S l) xs)::rest

-------------------

mergeLine : {n: Nat} -> Vect n Nat -> Vect n Nat
mergeLine {n=Z} [] = []
mergeLine {n=S Z} [x] = [x]
mergeLine {n=S (S k)} (x::x'::xs) = 
  if x == Z then rewrite plusCommutative 1 (S k) in (mergeLine (x'::xs))++[Z]
  else if x' == Z then rewrite plusCommutative 1 (S k) in (mergeLine (x::xs))++[Z]
  else if x == x' then rewrite plusCommutative 1 (S k) in ((x*2)::xs)++[Z]
  else x::mergeLine (x'::xs)

mergeLineLeft : {n: Nat} -> Vect n Nat -> Vect n Nat
mergeLineLeft = mergeLine

mergeLineRight : {n: Nat} -> Vect n Nat -> Vect n Nat
mergeLineRight = reverse . mergeLineLeft . reverse

vmap : (a -> b) -> Vect n a -> Vect n b
vmap f [] = []
vmap f (x::xs) = (f x)::(vmap f xs)

mergeLeft : {n: Nat} -> Vect n (Vect n Nat) -> Vect n (Vect n Nat)
mergeLeft = vmap mergeLineLeft

mergeRight : {n: Nat} -> Vect n (Vect n Nat) -> Vect n (Vect n Nat)
mergeRight = vmap mergeLineRight

mergeUp : {n: Nat} -> Vect n (Vect n Nat) -> Vect n (Vect n Nat)
mergeUp = transpose . mergeLeft . transpose

mergeDown : {n: Nat} -> Vect n (Vect n Nat) -> Vect n (Vect n Nat)
mergeDown = transpose . mergeRight . transpose

move : {n : Nat} -> Direction -> Vect n (Vect n Nat) -> Vect n (Vect n Nat)
move Left  = mergeLeft
move Right = mergeRight
move Up    = mergeUp
move Down  = mergeDown

emptyLine : (n: Nat) -> Vect n Nat
emptyLine n = Vect.replicate n Z

emptyGrid : (n: Nat) -> Vect n (Vect n Nat)
emptyGrid n = Vect.replicate n (emptyLine n)

winNumber : Nat
winNumber = 2048

isWin : {n: Nat} -> Vect n (Vect n Nat) -> Bool
isWin = orvmap (orvmap (== winNumber))

isLineFail : {n : Nat} -> Vect n Nat -> Bool
isLineFail [] = True
isLineFail [x] = True
isLineFail (x::x'::xs) = (not ((x == x') || (0 == x) || (0 == x'))) && (isLineFail (x'::xs))

isFail : Vect n (Vect n Nat) -> Bool
isFail g = (andvmap isLineFail g) && (andvmap isLineFail (transpose g))

vectSet : {n: Nat} -> Vect n a -> Nat -> a -> Vect n a
vectSet [] _ _ = []
vectSet (x::xs) Z ele = ele::xs
vectSet (x::xs) (S k) ele = x::(vectSet xs k ele)

showLine : {n: Nat} -> Vect n Nat -> String
showLine [] = ""
showLine (x::xs) = (show x) ++ " " ++ (showLine xs)

showGrid : Vect n (Vect m Nat) -> String
showGrid [] = ""
showGrid (l::ls) = (showLine l) ++ "\n" ++ (showGrid ls)

strToAction : String -> Action
strToAction "w" = Merge Up
strToAction "a" = Merge Left
strToAction "s" = Merge Down
strToAction "d" = Merge Right
strToAction "q" = Exit
strToAction  _  = Invalid

addRandom : {n: Nat} -> Vect n (Vect n Nat) -> Eff (Vect n (Vect n Nat)) [RND, EXCEPTION String]
addRandom {n=k} g = case !(rndSelect seeds) of
    Nothing => raise "Game Over"
    Just idx => return (partition {n=k} {m=k} (vectSet flattenedGrid idx 2))
  where 
    seeds : List Nat
    seeds = [2, 2, 2, 4]
    flattenedGrid : Vect (k*k) Nat 
    flattenedGrid = flatten {n=k} {m=k} g

loop : {n: Nat} -> Vect n (Vect n Nat) -> Eff (Vect n (Vect n Nat)) [RND, EXCEPTION String, SYSTEM, STDIO]
loop {n=k} g = 
  do putStrLn (showGrid g)
     c <- getStr
     doAction g (strToAction c)
   where doAction : {n: Nat} -> Vect n (Vect n Nat) -> Action -> Eff (Vect n (Vect n Nat)) [RND, EXCEPTION String, SYSTEM, STDIO]
         doAction g (Merge d) = 
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
         doAction g Exit = raise "Exit."
         doAction g Invalid = loop g

randomNumber : Eff Nat [RND, SYSTEM]
randomNumber = do srand !time
                  x <- rndInt 0 10000
                  return (fromInteger x)

startGame : Eff () [RND, SYSTEM, STDIO, EXCEPTION String]
startGame = do srand !time
               initGrid <- addRandom (emptyGrid 4)
               loop initGrid
               return ()

main : IO ()
main = run startGame

