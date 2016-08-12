{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.ByteString            (pack)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List
import           Network.Wreq

(<&<) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <&< g = g &&& f >>> arr (uncurry (&&))

(>&>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f >&> g = f &&& g >>> arr (uncurry (&&))

collatz :: Integer -> [Integer]
-- ^ Iterates the collatz step function until 1
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

collatzIndentity = oddBases 1 2 [0..]

-- | Subtracts out the Collatz Identity
extractIdents = flip (zipWith (-)) collatzIndentity

collatzFIdent b = extractIdents $ nOddBases b 2 10

(=|>) :: (a -> b) -> (a,a) -> (b,b)
(=|>) f (x,y) = (f x, f y)

-- | Divides two Collatz Identities by eachother
compareCollatzIdents = curry $ (=|>) (map fromIntegral . collatzFIdent) >>> uncurry (zipWith (/))

-- | The identity functions for odd numbers up to n
nCollatzFIdents n = map (collatzFIdent . (+) 1 . (*) 2) [0..n]


-- | Prints the identites of nCollatsFIdents
showNCollatzFIdents = mapM_ print . nCollatzFIdents

-- | Odd numbers
odds = filter odd [0..]

ordByFIdentCollatz :: Rational -- Amount of idents to find
                   -> [(Integer, [Integer])]
-- ^ Creates and sorts the collatz identity functions
ordByFIdentCollatz = nCollatzFIdents >>> zip odds
                 >>> filter ((/=) [] . snd)
                 >>> sortOn (snd . second head)

queryOEIS :: String -- ^ List of integers eg. 1,2,3,4
          -> IO [C.ByteString]
-- ^ Queries OEIS to find possible functions for the sequence
queryOEIS se = do
  r <- get $ "http://oeis.org/search?fmt=text&q=" ++ se
  return $ C.lines (r ^. responseBody)

checkOEIS :: [Integer] -- ^ Generated list of numbers
          -> IO ()
-- ^ Queries OEIS to find possible functions for the sequence
checkOEIS = map show >>> intercalate "," >>> queryOEIS
        >=> filter ("%N" `C.isPrefixOf`) >>> return
        >=> mapM_ print

someFunc :: IO ()
someFunc = putStrLn "someFunc"
