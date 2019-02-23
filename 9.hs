-- module Foo where
import Data.List
import Data.Ord
import qualified Data.Map as M

main = print $ part1 7158800 430

part1 totalTurns playerCount = maximumBy (comparing snd) . M.toList . M.fromListWith (+) $ points totalTurns playerCount

simTurn :: Int -> RingBuffer -> (RingBuffer, Int)
simTurn i r
  | i `mod` 23 == 0 = case pop (rotateLeft 7 r) of
      (r', o) -> (r', maybe i (+i) o)
  | otherwise =  (push i (rotateRight 2 r), 0)

points :: Int -> Int -> [(Int, Int)]
points totalTurns playerCount = unfoldr go (1, R [] [0])
  where
    go (turn, r)
        | turn > totalTurns = Nothing
        | otherwise =
                case simTurn turn r of
                  (r', points) -> Just ((toPlayer turn, points), (turn+1, r'))
    toPlayer turn = (turn-1) `mod` playerCount + 1

data RingBuffer = R [Int] [Int]

push :: Int -> RingBuffer -> RingBuffer
push a (R l r) = R l (a:r)
pop :: RingBuffer -> (RingBuffer, Maybe Int)
pop (R l (x:xs)) = (R l xs, Just x)
pop r = (r, Nothing)


rotateLeft :: Int -> RingBuffer -> RingBuffer
rotateLeft i = flip . rotateRight i . flip
  where
    flip (R l r) = R r l

rotateRight :: Int -> RingBuffer -> RingBuffer
rotateRight i (R l r) = go i l r
  where
      go 0 l r = R l r
      go i l (x:xs) = go (i-1) (x:l) xs
      go i [] [] = R [] []
      go i l [] = go i [] (reverse l)


