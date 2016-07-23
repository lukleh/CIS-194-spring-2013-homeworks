toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x < 0 = []
  | x < 10 = [x]
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleFromLeft . reverse
  where doubleFromLeft :: [Integer] -> [Integer]
        doubleFromLeft (x : y : xs) = x : (2 * y) : doubleFromLeft xs
        doubleFromLeft [] = []

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits. doubleEveryOther. toDigits

main :: IO ()
main = do
  print $ validate 4012888888881881 == True
  print $ validate 4012888888881882 == False
