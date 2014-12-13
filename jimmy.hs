
import           Control.Applicative   ((<$>))
import           Control.Arrow         ((&&&))
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (toLower)
import           Data.Char             (isUpper)
import           Data.List
import qualified Data.Map              as Map
import           System.Environment    (getArgs)

main = do
  letters <- concat <$> getArgs
  dict <- map (map toLower) . filter (\x -> isUpper (head x) && length x >=3  ) . lines <$> readFile "/usr/share/dict/words"
  putStr $ unlines $ map unwords $ search dict (inventory letters)

inventory = Map.fromList . map (head &&& length) . group . sort

search :: [String] -> Map.Map Char Int -> [[String]]
search [] _ = []
search (w:wordlist) inventory = case fits w inventory  of
  Nothing -> rest
  Just i -> if Map.null i
            then [[w]]
            else  (map (w:) (search (w:wordlist) i)) ++ rest
  where rest = search wordlist inventory

fits :: String -> Map.Map Char Int -> Maybe (Map.Map Char Int)
fits word inventory = foldr  get (Just inventory) word
  where get _ Nothing = Nothing
        get letter (Just i) =  case Map.lookup letter i of
          Nothing -> Nothing
          Just n -> case n of
            1 -> Just (Map.delete letter i)
            n -> Just (Map.adjust (+ (-1)) letter i)
