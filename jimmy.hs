{-# LANGUAGE BangPatterns #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       ((&&&))
import           Data.Char           (isUpper, toLower)
import           Data.Char           (isAlpha)
import           Data.List           (group, sort)
import           Data.Map            (lookup)
import           Data.Map            (fromList, keys)
import           Data.Maybe          (fromJust)

import           Data.Char           (isAscii)
import           Data.Int            (Int64)
import           Data.Map            (toList)
import           Data.Map            (fromListWith)
import           Debug.Trace         (trace)
import           Prelude             hiding (lookup, null)
import           System.Environment  (getArgs)

main = (search <$> (buildWords . lines <$> readFile "/usr/share/dict/words")
               <*> (buildInventory <$> getArgs))
            >>= print -- putStr . unlines . map unwords

buildWords :: [String] -> [(Int64, [String])]
buildWords l = let res = [(sig x,[x]) | r <- l, isUpper (head r), all isAlpha r, length r >= 3,
                          let x = map toLower r]
               in toList $ fromListWith (++) res
--buildWords =  map ((\x -> (sig x,x)) . map toLower) . filter (\x -> isUpper
--                          (head x) && length x >=3 && all isAlpha x) . lines

buildInventory :: [String] -> Int64
buildInventory = sig . concat


search :: [(Int64, [String])] -> Int64 -> [[[String]]]
search [] _ = []
search orig@((sigw,w):wordlist) inventory = maybe rest continue (numfits sigw inventory)
  where rest = search wordlist inventory
        continue i = if i==1 then [[w]] else map (w:) (search orig i)  ++ rest

numfits !s !inventory = case inventory `quotRem` s of
  (!n,!0) -> Just n
  _ -> Nothing



primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]


sig  = product . map (\(c,n) ->  (fromJust $ lookup c costs) ^ n ) . lettercount
costs = fromList $ zip ['a' .. 'z'] primes

lettercount = map (head &&& length ) . group . sort . filter isAscii . map toLower
