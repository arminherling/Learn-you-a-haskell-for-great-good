lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe x = "Not between 1 and 3"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."

bmiTell' :: Double -> Double -> String
bmiTell' weight height  -- height is meters, e.g. 1.75
    | weight / height ^ 2 <= 18.5 = "You're underweight, eat more!"
    | weight / height ^ 2 <= 25.0 = "Looking good!"
    | weight / height ^ 2 <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b    = EQ
    | a <= b    = LT
    | otherwise = GT
    
bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2

bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny     = "You're underweight, eat more!"
    | bmi <= normal     = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          overweight = 30.0

bmiTell'''' :: Double -> Double -> String
bmiTell'''' weight height
    | bmi <= skinny     = "You're underweight, eat more!"
    | bmi <= normal     = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          (skinny, normal, overweight) = (18.5, 25.0, 30.0)
          


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname


calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [ bmi w h | (w, h) <- xs ]
    where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2 ]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x


describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a long list."

