{-# LANGUAGE BangPatterns #-}
import           Control.Applicative   ((<$>), (<*>))
import           Control.Arrow         ((&&&))
import           Data.Char             (isUpper, toLower)
import           Data.Char             (isAlpha)
import           Data.List             (group, sort)
import           Data.Map              (lookup)
import           Data.Map              (fromList, keys)
import           Data.Maybe            (fromJust)

import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAscii)
import           Data.Int              (Int64)
import           Data.Map              (toList)
import           Data.Map              (fromListWith)
import           Debug.Trace           (trace)
import           Prelude               hiding (lookup, null)
import           System.Environment    (getArgs)


main = (search <$> (buildWords . BS.lines <$> BS.readFile "/usr/share/dict/words")
               <*> (buildInventory <$> getArgs))
            >>= BS.putStr . BS.unlines . map BS.unwords

buildWords :: [BS.ByteString] -> [(Int64, BS.ByteString)]
buildWords l =[(bsSig x,x) | r <- l, isUpper (BS.head r), BS.all isAlpha r, BS.length r >= 3,
               let x = BS.map toLower r]

buildInventory :: [String] -> Int64
buildInventory = sig . concat

search :: [(Int64, BS.ByteString)] -> Int64 -> [[BS.ByteString]]
search [] _ = []
search orig@((sigw,w):wordlist) !inventory = case inventory `quotRem` sigw of
  (n,0) -> if n == 1 then [[w]] else rest ++ map (w:) (search orig n)
  _ -> rest
  where rest = search wordlist inventory

primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]

sig  = product . map (\(c,n) ->  (fromJust $ lookup c costs) ^ n ) . lettercount

bsSig = BS.foldl' (\a c -> fromJust (lookup c costs) * a) 1
costs = fromList $ zip letters primes

lettercount = map (head &&& length ) . group . sort . filter isAscii . map toLower

letters = map toLower "ETAONRISHDLFCMUGYPWBVKJXQZ"
