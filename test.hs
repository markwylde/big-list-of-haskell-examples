module Test (favNums) where

import Data.List
import System.IO

-- Some IO
sayHelloAgain = do
  putStrLn "What's your name"
  name <- getLine
  putStrLn $ "Hello " ++ name

writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("This is a test")
  hClose theFile

readFromFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile
  putStr contents
  hClose theFile

-- Type Classes
data Employee = Employee { name :: String
                         , position :: String
                         , idNum :: Int
                         } deriving (Eq, Show)

samSmith = Employee { name = "Sam Smith", position = "Manager", idNum = 1}
pamMaxis = Employee { name = "Pam Maxis", position = "Sales", idNum = 2}

isSamPam = samSmith == pamMaxis

samSmithData = show samSmith

data ShirtSize = S | M | L
instance Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "Small"
  show M = "Medium"
  show L = "Large"

smallAvail = S `elem` [S, M, L]

theSize = show S

-- 1

data BaseballPlayer = Pitcher 
                    | Catcher
                    | Infielder
                    | Outfield
                  deriving Show

barryBonds :: BaseballPlayer -> Bool
barryBonds Outfield = True

barryInOF = print(barryBonds Outfield)

-- 2

data Customer = Customer String String Double
  deriving Show

tomSmith :: Customer
tomSmith = Customer "Tom Smith" "123 Main" 20.50

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b

-- 3

data RPS = Rock | Paper | Scissors

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper Beats Rock"
shoot Rock Scissors = "Rock Beats Scissors"
shoot _ _ = "Error"

-- 4

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving Show

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x y x2 y2) = (abs (x2 - x)) * (abs (y2 - y))

-- 5

sumValue = putStrLn (show (1 + 2))
sumValue2 = putStrLn . show $ 1 + 2

areaOfCircle = area $ Circle 50 60 20
areaOfRect = area $ Rectangle 10 10 10 10

-- Examples

primeNumbers = [3,5,7,11]
morePrimes = primeNumbers ++ [12,17,19,23,29]

favNums = 2 : 7 : 21 : 66 : []

multiList = [[1,2,3],[4,5,6]]
flattened = concat multiList

revPrime = reverse flattened

isListEmpty = null revPrime

secondPrime = morePrimes !! 1

firstPrime = head morePrimes
lastPrime = last morePrimes

primeInit = init morePrimes

first2Prime = take 3 morePrimes
removeFirst3Primes = drop 3 morePrimes

dropReverse n el =
  take ((length el) - n) morePrimes

removeLast3Primes = dropReverse 3 morePrimes

is7InList = 7 `elem` morePrimes
is6InList = 6 `elem` morePrimes

themax = maximum [5,7,9,55,1,6]

newList = [5..10]
productPrime = product newList

evenList = [2,4..100]
letterList = ['A','C'..'Z']

infinPow10 = [10,20..]

many2s = take 10 (repeat 2)

listTimes2 = [x * 2 | x <- [1..10]]
listTimes3 = [x * 3 | x <- [1..10], x * 3 <= 50]

sortedList = sort [6,3,8,4,7,4]

sumOfLists = zipWith (+) [1,2,3,4,5] [6,7,8,9,10]

listBiggerThan5 = filter (>5) morePrimes

eventsUpTo20 = takeWhile (<= 20) [2,4..]

doTimes :: Int -> Int -> Int
doTimes a b = a * b

multipleOfList = foldl doTimes 1 [2,3,4,5]

powerOf3List = [3^n | n <- [1..10]]

multiTable = [[x * y | y <- [1..10]] | x <- [1..10]]

-- It's all about tuples

randTuple = (1, "Random 10")

bobSmith = ("Bob Smith", 52)

bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"] 
addresses = ["123 Main", "234 North", "567 South"]

namesNAddress = zip names addresses

-- It's all about functions

sayHello = do
  putStrLn "What's your name?"
  name <- getLine
  putStrLn ("Hello " ++ name)


addMe :: Int -> Int -> Int
addMe a b = a + b

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x, y) (x2, y2) =
  (x + x2, y + y2)

whatAge :: Int -> String
whatAge 16 = "You can drive"
whatAge 18 = "You can vote"
whatAge 21 = "You can drink"
whatAge _ = "Whatever"


isOdd :: Int -> Bool
isOdd n
    | n `mod` 2 == 0 = False
    | otherwise = True

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

-- All about Gaurds
whatGrade :: Int -> String
whatGrade age
  | (age < 5) = "Not ready for education yet"
  | (age >= 5) && (age <= 6) = "Kindergarten"
  | (age >= 6) && (age <= 10) = "Elementary School"
  | (age >= 10) && (age <= 14) = "Middle School"
  | (age >= 14) && (age <= 18) = "High School"
  | otherwise = "Go to college"

batAvgRating :: Double -> Double -> String
batAvgRating hits atBats
  | avg <= 0.200 = "Terrible Batting Average"
  | avg <= 0.250 = "Average Player"
  | avg <= 0.280 = "Good"
  | otherwise = "You're a superstar"
  where avg = hits / atBats


-- Some about lists
getListItems :: [Int] -> String
getListItems [] = "Your list is empty"
getListItems (x:[]) = "Your list starts with " ++ show x
getListItems (x:y:[]) = "Your list contains " ++ show x ++ " and " ++ show y
getListItems (x:xs) = "The first item is " ++ show x ++ " and the rest is " ++ show xs



getFirstItem :: String -> String
getFirstItem [] = "Empty String"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ [x]


-- Higher order functions

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1,2,3,4,5]

multiplyBy4 :: [Int] -> [Int]
multiplyBy4 [] = []
multiplyBy4 (x:xs) = times4 x : multiplyBy4 xs


areStringsEq :: [Char] -> [Char] -> Bool
areStringsEq [] [] = True
areStringsEq (x:xs) (y:ys) = x == y && areStringsEq xs ys
areStringsEq _ _ = False




doMulti :: (Int -> Int) -> Int
doMulti func = func 3

num3Times4 = doMulti times4

dbl1To10 = map (\x -> x * 2) [1..10]

-- < > <= >= == /=
-- && || not

doubleEvenNumber y =
  if (y `mod` 2 /= 0)
    then y
    else y * 2

doTheDoubles = map doubleEvenNumber [1..10]

getClass :: Int -> String
getClass n = case n of 
  5 -> "Go to Kindergarten"
  6 -> "Go to Elementary"
  _ -> "Go away"










