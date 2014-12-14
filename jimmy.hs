{-# LANGUAGE BangPatterns #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       ((&&&))
import           Data.Char           (isUpper, toLower)
import           Data.Char           (isAlpha)
import           Data.List           (group, sort)
import           Data.Map            (lookup)
import           Data.Map            (fromList, keys)
import           Data.Maybe          (fromJust)

import qualified Data.ByteString     as BS
import           Data.Char           (isAscii)
import           Data.Int            (Int64)
import           Data.Map            (toList)
import           Data.Map            (fromListWith)
import           Debug.Trace         (trace)
import           Prelude             hiding (lookup, null)
import           System.Environment  (getArgs)


main = (search <$> (buildWords . lines <$> readFile "/usr/share/dict/words")
                 <*> (buildInventory <$> getArgs))
            >>= putStr . unlines . map unwords

buildWords :: [String] -> [(Int64, String)]
buildWords l =[(sig x,x) | r <- l, isUpper (head r), all isAlpha r, length r >= 3,
               let x = map toLower r]
--               in toList $ fromListWith (++) res
--buildWords =  map ((\x -> (sig x,x)) . map toLower) . filter (\x -> isUpper
--                          (head x) && length x >=3 && all isAlpha x) . lines

buildInventory :: [String] -> Int64
buildInventory = sig . concat


search :: [(Int64, String)] -> Int64 -> [[String]]
search [] _ = []
search orig@((sigw,w):wordlist) !inventory = case inventory `quotRem` sigw of
  (n,0) -> if n == 1 then [[w]] else rest ++ map (w:) (search orig n)
  _ -> rest
  where rest = search wordlist inventory


primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]


sig  = product . map (\(c,n) ->  (fromJust $ lookup c costs) ^ n ) . lettercount
costs = fromList $ zip letters primes

lettercount = map (head &&& length ) . group . sort . filter isAscii . map toLower


-- we use
--letters = ['a' .. 'z']
letters = map toLower "ETAONRISHDLFCMUGYPWBVKJXQZ"
