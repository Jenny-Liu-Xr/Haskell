-- Xiaorui Liu
-- 101099674


-- Remove any and all occurrences of the letters H or W that occur after the first character
remove_hw :: String -> String
remove_hw []       = []
remove_hw (x : xs) = x : filter (\c -> not (elem c "HW")) xs

-- Remove any duplicate letters
remove_duplicate :: String -> String
remove_duplicate []           = []
remove_duplicate [x]          = [x]
remove_duplicate (x : y : ys)
  | x == y    = remove_duplicate (y : ys)
  | otherwise = x : remove_duplicate (y : ys)

-- Remove all occurrences of the letters A, E, I, O, U, or Y that occur after the first character
remove_aeiouy :: String -> String
remove_aeiouy []       = []
remove_aeiouy (x : xs) = x : filter (\c -> not (elem c "AEIOUY")) xs

-- Convert every character after the first to a numerical digit
convert :: String -> String
convert []       = []
convert (x : xs) = x : convert_helper xs

convert_helper :: String -> String
convert_helper []       = []
convert_helper (x : xs)
  | elem x "BFPV"     = '1' : convert_helper xs
  | elem x "CGJKQSXZ" = '2' : convert_helper xs
  | elem x "DT"       = '3' : convert_helper xs
  | elem x "L"        = '4' : convert_helper xs
  | elem x "MN"       = '5' : convert_helper xs
  | elem x "R"        = '6' : convert_helper xs
  | otherwise         = x : convert_helper xs

-- Make string exactly four characters in length
exactly :: String -> String
exactly = exactly_helper 4

exactly_helper :: Int -> String -> String
exactly_helper 0 _        = []
exactly_helper n []       = '0' : exactly_helper (n - 1) []
exactly_helper n (x : xs) = x : exactly_helper (n - 1) xs

-- Producing the Soundex code associated with a name
soundex :: String -> String
soundex name = (exactly . convert . remove_aeiouy . remove_duplicate . remove_hw) name

--  a list of tuples wherein the first element will be a last name and the second element will be a phone number
type Database = [(String, String)]

query :: Database -> String -> [(String)]
query database name = map snd (filter (\(x, _) -> soundex x == soundex name) database)

main = do putStrLn (soundex "COLLIER")
          putStrLn (soundex "COLLIER")
          putStrLn (show (query [("COLYER", "613-520-2600"), ("COLLIE", "613-520-2601")] "COLLIER"))
