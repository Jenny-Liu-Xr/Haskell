import Data.Maybe

-- first recursive long division function use error
fstFunc :: Integer -> Integer -> (Integer, Integer)
fstFunc x y
  | y == 0 = error "Divison by zero"
  | x < y = (0, x)
  | otherwise = (q + 1, r)
      where (q, r) = fstFunc (x - y) y

-- second recursive long division function
-- A data type to represent the result
data Result a = Division a a | Zero deriving (Show)

-- The long division function use Maybe 
sndFunc :: Integer -> Integer -> Maybe (Integer, Integer)
sndFunc x y
  | y == 0 = Nothing
  | x < y = Just (0, x)
  | otherwise = case sndFunc (x - y) y of
                  Nothing -> Nothing
                  Just (q, r) -> Just (q + 1, r)

-- selector functions
quotient :: Result a -> Maybe a
quotient (Division q _) = Just q
quotient Zero = Nothing

remainder :: Result a -> Maybe a
remainder (Division _ r) = Just r
remainder Zero = Nothing


-- third recursive long division function
-- The long division function use tail recursion
tirFunc :: Integer -> Integer -> (Integer, Integer)
tirFunc x y = helper x y 0

-- helper function
helper :: Integer -> Integer -> Integer -> (Integer, Integer)
helper x y q 
  | x < y = (q, x)
  | otherwise = helper (x - y) y (q + 1)


