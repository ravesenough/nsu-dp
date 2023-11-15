module Solution
    ( unique
    , pythagoreanTriples
    , primitivePythagoreanTriples
    , perfectNumbers
    , cantorPairs
    , minimalDistance
    ) where

unique :: Eq a => [a] -> Bool
unique [] = True
unique [x] = True
unique (x:xs)
    | x `elem` xs = False
    | otherwise = unique (xs)


pythagoreanTriples :: Integral a => [(a, a, a)]
pythagoreanTriples = [(b, a, c) |  c <- [5..], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]


primitivePythagoreanTriples :: Integral a => [(a, a, a)]
primitivePythagoreanTriples = [(a, b, c)
            | (a, b, c) <- pythagoreanTriples, gcd b c == 1 && gcd b a == 1 && gcd c a == 1]


isNumberPerfect :: Integral a => a -> Bool
isNumberPerfect a = sum [x | x <- [1..a - 1], mod a x == 0] == a
perfectNumbers :: Integral a => [a]
perfectNumbers = [a | a <- [1..], isNumberPerfect a]


cantorPairs :: Integral a => [(a, a)]
cantorPairs = undefined


minimalDistance :: RealFloat a => [(a, a)] -> a
minimalDistance [] = 1 / 0
minimalDistance [_] = 1 / 0
minimalDistance l = undefined
