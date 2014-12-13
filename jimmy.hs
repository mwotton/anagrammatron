{-# LANGUAGE BangPatterns #-}
import           Control.Applicative ((<$>), (<*>))
import           Control.Arrow       ((&&&))
import           Data.Char           (isUpper, toLower)
import           Data.List           (group, sort)
import           Data.Map.Strict     (delete, fromList, insert, lookup, null)
import           Prelude             hiding (lookup, null)
import           System.Environment  (getArgs)

main = (search <$> (buildWords <$> readFile "/usr/share/dict/words") <*> (buildInventory <$> getArgs))
            >>= putStr . unlines . map unwords
  where buildInventory = fromList . map (head &&& length) . group . sort . concat
        buildWords = map (map toLower) . filter (\x -> isUpper (head x) && length x >=3) . lines

search [] _ = []
search orig@(w:wordlist) inventory = maybe rest continue (fits w inventory)
  where rest = search wordlist inventory
        continue i = if null i then [[w]] else map (w:) (search orig i) ++ rest

fits (!word) (!inventory) = foldr  get (Just inventory) word
  where get letter store = store >>= \i -> lookup letter i >>= \n ->
          return $ (if n == 1 then delete else flip insert (n-1)) letter i
