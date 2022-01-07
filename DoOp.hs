import System.Exit
import System.Environment
import Data.Char
import Data.Maybe
import Prelude
import Control.Monad
import System.Directory.Internal.Prelude (getArgs)

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs)
    | x == a = True
    | otherwise = (myElem a xs)

safeDiv :: Int -> Int -> Maybe Int
safeDiv a 0 = Nothing
safeDiv a b = Just (div a b)

safeNth :: [a] -> Int -> Maybe a
safeNth [] a = Nothing
safeNth (x:xs) 0 = Just x
safeNth (x:xs) a = safeNth xs (a - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just a) = (Just (a+1))

myFst :: (a, b) -> a
myFst (a, _) = a

mySnd :: (a, b) -> b
mySnd (_, b) = b

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup a [] = Nothing
myLookup a (x:xs)
    | (myFst x) == a = Just (mySnd x)
    | otherwise = myLookup a xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo fct Nothing b = Nothing
maybeDo fct a Nothing = Nothing
maybeDo fct (Just a) (Just b) = Just (fct a b)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt (x:xs)
    | all isDigit (x:xs) == True = Just (read (x:xs) :: Int)
    | x == '-' && all isDigit xs == True = Just ((read xs :: Int) * (-1))
    | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
    line <- getLine
    return $ length line

printAndGetLength :: String -> IO Int
printAndGetLength s = do
    putStrLn s
    return $ length s

printBox :: Int -> IO ()
printBox 1 = do
    putStr "+\n"
printBox nb = do
    if (nb <= 0)
        then return ()
    else do
        putStr (concat ["+", (replicate ((nb*2)-2) '-'), "+\n"])
        putStr $ concat $ replicate (nb-2) (concat ["|", (replicate ((nb*2)-2) ' '), "|\n"])
        putStr (concat ["+", (replicate ((nb*2)-2) '-'), "+\n"])

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) [] = (x:xs)
myAppend (x:xs) y = x: (myAppend xs y)

getLines :: Int -> IO [String]
getLines x = replicateM x getLine

concatLines :: Int -> IO String
concatLines nb = do
    line <- (getLines nb)
    return (concat line)

getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    return (readInt line)


myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myNth :: [a] -> Int -> a
myNth [] y = error "Empty list"
myNth (x:xs) y
    | (myLength (x:xs)) < y = error "Too large index"
    | (myIsNeg y) == True = error "Negative index"
myNth (x:xs) 0 = x
myNth (x:xs) y = myNth xs (y-1)

main :: IO ()
main = do
    args <- getArgs
    case myLength args /= 3  myElem (myNth args 1) ["+","-","/","*","%"] == False
         readInt (myNth args 0) == Nothing || readInt (myNth args 2) == Nothing of
        True  -> exitWith (ExitFailure 84)
        False  -> pure ()
    case myNth args 1 of
        "+" -> print $ myReadInt (myNth args 0) + myReadInt (myNth args 2)
        "-" -> print $ myReadInt (myNth args 0) - myReadInt (myNth args 2)
        "%" -> print $ mod myReadInt (myNth args 0) myReadInt (myNth args 2)
        "/" -> print $ div myReadInt (myNth args 0) myReadInt (myNth args 2)
        "*" -> print $ myReadInt (myNth args 0) * myReadInt (myNth args 2)
        _ -> print "Nothing"