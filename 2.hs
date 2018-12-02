{-# Language TupleSections #-}
import qualified Data.Map as M
main :: IO ()
main = do
    content <- lines <$> readFile "2.txt"
    let occs = map countElems content
    print $ count (has 2) occs * count (has 3) occs
    print $ findBox content
  where
    has b = elem b . M.elems
    countElems = M.fromListWith (+) . map (,1)
    count pred = length . filter pred
    -- 250 * 250 = 62500
    findBox ls = [sameLetters a b | a <- ls, b <- ls, different a b == 1]
    different a b = count not (zipWith (==) a b)
    sameLetters a b = map fst $ filter (uncurry (==)) $ zip a b

