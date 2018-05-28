doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]
sequence50To100Remainder3 = [ x | x <- [50..100], x `mod` 7==3 ]
sequenceNot13Not15Not19 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19 ]
combingTwoLists = [ x+y | x <- [1,2,3], y <- [10,100,1000] ]
combingTwoLists' = [ x*y | x <- [2,5,10], y <- [8,10,11] ]
combingTwoLists'' = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50 ]

nouns = ["hobo", "frog", "pope"]
adjectives = ["lazy", "grouchy", "scheming"]
wordCombinations = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

lenght' xs = sum [ 1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]
nestedLists = [[1,3,5,2,3,1,2,4,5], [1,2,3,4,5,6,7,8,9], [1,2,4,2,1,6,3,1,3,2,3,6]]
evenNumbersInNestedLists xxs = [ [ x | x <- xs, even x] | xs <- xxs ]

zippedLists = zip [1..5] [5,5,5,5,5]
zippedLists' = zip [1..5] ["one", "two", "three", "four", "five"]
zippedListsWithDifferentLenghts = zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
zippedInfiniteListWithFiniteList = zip [1..] ["apple", "orange", "cherry", "mango"]

triangles = [ (a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10] ] -- creates list of 1000 tuples
rightTriangles = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2 ]
rightTriangles' = [ (a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c == 24 ]
