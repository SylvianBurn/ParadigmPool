
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
myNth [] y = error "Empty list"
myNth (x:xs) y
    | (myLength (x:xs)) < y = error "Too large index"
    | (myIsNeg y) == True = error "Negative index"
myNth (x:xs) 0 = x
myNth (x:xs) y = myNth xs (y-1)

myTake :: Int -> [a] -> [a]
myTake index [] = error "Empty list"
myTake index (x:xs)
    | (myIsNeg index) == True = error "Index is negative"
    | (myLength (x:xs)) <= index = (x:xs)
myTake 0 (x:xs) = []
myTake index (x:xs) = x: (myTake (index - 1) xs)

myDrop :: Int -> [a] -> [a]
myDrop index [] = error "Empty list"
myDrop index (x:xs)
    | (myIsNeg index) == True = error "Negative index"
    | (myLength (x:xs)) <= index = []
myDrop 0 (x:xs) = (x:xs)
myDrop index (x:xs) = myDrop (index - 1) xs

myAppend :: [a] -> [a] -> [a]
myAppend [] y = y
myAppend (x:xs) [] = (x:xs)
myAppend (x:xs) y = x: (myAppend xs y)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myAppend(myReverse xs) [x]

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit x = myReverse (myDrop 1 (myReverse x))

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast x = myHead (myDrop ((myLength x)-1) x)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = myTuple x y : myZip xs ys

myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((a, b) : y) = myTuple (a : myFst (myUnzip y)) (b : mySnd (myUnzip y))