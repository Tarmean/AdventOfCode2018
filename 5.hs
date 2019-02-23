{-# OPTIONS_GHC -fdefer-type-errors #-}
import qualified Data.Map as M
import Data.Ord
import Data.List
import Data.Char

main :: IO ()
main = do
    content <- readFile "5.txt"
    let content' = head (lines content)
    print (solutionA content')
    print (solutionB content')

solutionB ls = minimum [solutionA (tryRemoving c ls) | c <- ['a'..'z']]
  where tryRemoving c = filter ((/= c) . toLower)
solutionA = length . foldr step []
  where
    step x (y:acc)
      | isPair x y = acc
      | otherwise = x:y:acc
    step x [] = [x]
isPair a b = a /= b && toLower a == toLower b
