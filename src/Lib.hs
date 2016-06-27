module Lib
    ( someFunc
    ) where

import           Control.Arrow
import           Data.List

(<&<) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <&< g = g &&& f >>> arr (uncurry (&&))

(>&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f >&> g = f &&& g >>> arr (uncurry (&&))

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

collatzOddsFromInterN b n =  filter nonFractional . fmap (collatzIterN b n (const 1.0))

-- | Generates an amount of odds from iteration n on the branch with base b
nCollatzOddsFromIterN b amount n =  take amount $ collatzOddsFromInterN b n [0..]

-- | Generates the iterative connecting odds for the branch with base b
nCollatzOdds b amount n = nub $ sort $ concatMap (nCollatzOddsFromIterN b amount) [1..n]

oddBases b n = filter ((odd . round) <&< nonFractional) . fmap (collatzIterN b n (const 1.0))

firstOddBase b n = (round . head . oddBases b n) [0..]

terminalOdds b n = filter ((==) 0 . flip mod 3 . round) . oddBases b n

firstTerminalOdd b n = (round . head . terminalOdds b n) [0..]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
