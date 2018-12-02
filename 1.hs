import qualified Data.Set as S
import Data.List
main :: IO ()
main = do
    input <- map readInt . lines <$> readFile "1.txt"
    print (sum input)
    print $ findRepeat $ scanl (+) 0 $ cycle input
  where
    readInt ('+':d) = read d
    readInt d = read d

findRepeat :: Ord a => [a] -> Maybe a
findRepeat = fmap fst . find (uncurry S.member) . (zip <*> previous)
    where previous = scanl (flip S.insert) mempty
