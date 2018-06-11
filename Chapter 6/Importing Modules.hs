import Data.List
import Data.Char

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordNums :: String -> [(String,Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words

-- same as isInfixOf in Data.List
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

stringToListOfNumbers :: [Char] -> [Int]
stringToListOfNumbers xs = map ord xs

encode :: Int -> String -> String
encode offset msg = map (\c -> chr $ ord c + offset) msg

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg
