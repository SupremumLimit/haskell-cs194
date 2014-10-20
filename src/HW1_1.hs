-- | Main entry point to the application.
module HW1_1 where

import Data.List 
import Data.Char

split [] = ([], [])
split [x] = ([x], [])
split (x:y:xs) = (x:xp, y:yp) where (xp, yp) = split xs

--double [] = []
--double (x:xs) = 2 * x : double xs

double = map (\ x -> 2 * x)

toInt = map digitToInt

--toDigits :: Integral x => x -> [x]
--toDigits 0 = []
--toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigits n = if q == 0 then [r] else toDigits q ++ [r]
    where (q, r) = n `divMod` 10

listToDigits x = concat $ map toDigits x

--isValid :: [Char] -> Bool
isValid x = 
    let tup = split $ reverse x
        digits = listToDigits ((toInt $ fst tup) ++ (double $ toInt $ snd tup))
        in mod (sum digits) 10 == 0

--validate :: String -> String
validate str = if isValid str then "Valid" else "Not valid" 

validateInput input = unlines (map validate (lines input))

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Hello!"
    interact validateInput
