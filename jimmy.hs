import           Control.Applicative ((<$>))
import           Control.Arrow       ((&&&))
import           Data.Char           (isUpper, toLower)
import           Data.List           (group, sort)
import qualified Data.Map.Strict     as Map
import           System.Environment  (getArgs)

main = do
  letters <- concat <$> getArgs
  dict <- map (map toLower) . filter (\x -> isUpper (head x) && length x >=3  ) . lines
          <$> readFile "/usr/share/dict/words"
  putStr . unlines . map unwords . search dict $
    (Map.fromList . map (head &&& length) . group $ sort letters)

search [] _ = []
search (w:wordlist) inventory = maybe rest continue (fits w inventory)
  where rest = search wordlist inventory
        continue i = if Map.null i then [[w]] else (map (w:) (search (w:wordlist) i)) ++ rest

fits word inventory = foldr  get (Just inventory) word
  where get letter store = store >>= \i -> Map.lookup letter i >>= \n ->
          return $ (if n == 1 then Map.delete else Map.adjust (+ (-1))) letter i
