
mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myAbs :: Int -> Int
myAbs x
    | x < 0 = -x
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y
    | x < y = x
    | otherwise = y

myMax :: Int -> Int -> Int
myMax x y
    | x > y = x
    | otherwise = y

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, _) = a

mySnd :: (a, b) -> b
mySnd (_, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "Empty list"
myHead (x:xs) = x

myTail :: [a] -> [a]
myTail [] = error "Empty list"
myTail (x:xs) = xs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myNth :: [a] -> Int -> a
myNth (x:xs) y
    | (myLength (x:xs)) < y = error "Too large index"
    | (myIsNeg y) == True = error "Negative index"
    | y == 0 = x
    | otherwise = myNth xs (y-1)