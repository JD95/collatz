module Lib
    ( someFunc
    ) where

import           Control.Arrow
import           Data.List

(<&<) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <&< g = g &&& f >>> arr (uncurry (&&))

(>&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f >&> g = f &&& g >>> arr (uncurry (&&))

collatz :: Integer -> [Integer]
collatz = takeWhile (1 /=) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

collatzOdds = filter odd . collatz

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

oddBases b n
  | round b `mod` 3 /= 0 = fmap round . filter ((odd . round) <&< nonFractional) . fmap (collatzIterN b n (const 1.0))
  | otherwise = const []

nOddBases b n amount = take amount $ oddBases b n [0..]

firstOddBase b n = (head . oddBases b n) [0..]

terminalOdds b n = filter ((==) 0 . flip mod 3 . round) . oddBases b n

firstTerminalOdd b n = (head . terminalOdds b n) [0..]

extractIdents = flip (zipWith (-)) (oddBases 1 2 [0..])

collatzIndentity = oddBases 1 2 [0..]

collatzFIdent b = extractIdents $ nOddBases b 2 10

compareCollatzIdents b1 b2 = zipWith (/) (map fromIntegral $ collatzFIdent b1) (map fromIntegral $ collatzFIdent b2)

nCollatzFIdents n = map (collatzFIdent . \n->2*n+1) [0..n]

showNCollatzFIdents = mapM_ print . nCollatzFIdents

odds = filter odd [0..]

ordByFIdentCollatz = sortOn snd . map (second head) . filter ((/=) [] . snd) . zip odds . nCollatzFIdents

someFunc :: IO ()
someFunc = putStrLn "someFunc"
