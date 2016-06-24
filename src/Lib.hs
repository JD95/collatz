module Lib
    ( someFunc
    ) where

import           Data.List

-- | A utility for filtering out fractional numbers
nonFractional d = (fromIntegral $ ceiling d :: Rational) == d

-- | The iterative transformations of a branch to get the connecting odds
-- b is the base odd for the branch
-- f is the previous iteration
collatzIter b f n = (b * (2^n) * f n - 1) / 3

-- | Iterates the collatz function n times with base odd b
collatzIterN b n = last . take n . iterate (collatzIter b)

collatzIterNIndex :: Rational -> Int -> [(Int, Rational)]
-- ^ Generates the connecting odds along with their index for base odd b
collatzIterNIndex b n = filter (nonFractional . snd) $ zip [0..] $ fmap (collatzIterN b n (const 1.0)) [0..]

-- | Generates an amount of odds from iteration n on the branch with base b
nCollatzOddsFromIterN b amount n = take amount $ filter nonFractional $ fmap (collatzIterN b n (const 1.0)) [0..]

-- | Generates the iterative connecting odds for the branch with base b
nCollatzOdds b amount n = nub $ sort $ concatMap (nCollatzOddsFromIterN b amount) [1..n]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
