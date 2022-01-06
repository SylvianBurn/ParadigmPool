import System.Exit
import System.Environment
import Data.Char

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
    print line
    return $ length line
