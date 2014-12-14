{-# LANGUAGE BangPatterns #-}
import           Control.Applicative   ((<$>), (<*>))
import           Control.Arrow         ((&&&))
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAlpha, isAscii, isUpper, toLower)
import           Data.Int              (Int64)
import           Data.List             (group, sort)
import           Data.Map              (fromList, lookup)
import           Data.Maybe            (fromJust)
import           Data.Vector           (Vector, (!))
import qualified Data.Vector           as V
import           Prelude               hiding (lookup, null)
import           System.Environment    (getArgs)

main :: IO ()
main = (search <$> (buildWords . BS.lines <$> BS.readFile "/usr/share/dict/words")
               <*> (buildInventory <$> getArgs))
            >>= BS.putStr . BS.unlines . map BS.unwords

buildWords :: [BS.ByteString] -> Vector (Int64, BS.ByteString)
buildWords l = V.fromList [(bsSig x,x) | r <- l, isUpper (BS.head r), BS.all isAlpha r, BS.length r >= 3,
               let x = BS.map toLower r]

buildInventory :: [String] -> Int64
buildInventory = sig . concat

search :: Vector (Int64, BS.ByteString) -> Int64 -> [[BS.ByteString]]
search v invent = go (V.length v - 1) invent where
  go (-1) _ = []
  go index !inventory =
    let (sigw,w) = v ! index in
    case inventory `quotRem` sigw of
      (n,0) -> if n == 1 then [[w]] else rest ++ map (w:) (go (index-1) n)
      _ -> rest
    where rest = go (index - 1) inventory

primes :: [Int64]
primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101]

sig :: String -> Int64
sig  = product . map (\(c,n) ->  (fromJust $ lookup c costs) ^ n ) . lettercount

bsSig :: BS.ByteString -> Int64
bsSig = BS.foldl' (\a c -> fromJust (lookup c costs) * a) 1
costs = fromList $ zip letters primes

lettercount :: String -> [(Char, Int)]
lettercount = map (head &&& length ) . group . sort . filter isAscii . map toLower

letters :: String
letters = map toLower "ETAONRISHDLFCMUGYPWBVKJXQZ"
