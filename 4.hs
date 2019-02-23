{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE RecordWildCards #-}
{-# Language TupleSections #-}
module Foo where
import qualified Data.Map as M
import Text.Megaparsec as P
import Data.Ord
import Text.Megaparsec.Char
import Data.List
import Data.Void
import Data.Char
import Data.Monoid

main :: IO ()
main = do
    content <- readFile "4.txt"
    let e = parseAll content
    let g = fmap byMinute $ groupByGuard e
    let (guard, mins) = bestest (sum . M.elems) g
    let (min, amount) = bestest id mins
    print (guard * min)
    let bestMins = fmap (bestest id) g
        (guard, (min, amount)) = bestest snd bestMins
    print (guard * min)

groupByGuard = M.fromListWith (++) . map (\e -> (guard e, [e]))
byMinute :: [Event] -> M.Map Int (Sum Int)
byMinute  = M.fromListWith (+) . concatMap (\Event{..}-> map (,Sum 1) [begin..end-1])

bestest :: (Ord o) => (b -> o) -> M.Map a b -> (a,b)
bestest f = maximumBy (comparing (f . snd)) . M.toList

parseEvent :: Parser [Event]
parseEvent = do
    parseTimestamp
    word "Guard #"
    guard <- parseInt
    word "begins shift"
    many $ try $ do
        begin <- parseTimestamp
        word "falls asleep"
        end <- parseTimestamp
        word "wakes up"
        return $ Event{..}
parseAll :: String -> [Event]
parseAll ls = case runParser (many parseEvent) "" ls  of
    Right x -> concat x
    Left err -> error (parseErrorPretty err)
parseInt :: Parser Int
parseInt = (read <$> takeWhile1P Nothing isDigit) <* many (satisfy isSpace)
type Parser = Parsec Void String
parseTimestamp :: Parser Int
parseTimestamp = do
    char' '['
    parseInt
    char' '-'
    parseInt
    char' '-'
    parseInt
    parseInt
    char' ':'
    r <- parseInt
    char' ']'
    skip
    return r
data Event = Event {guard :: Int, begin :: Int, end :: Int}
  deriving Show
word ls = string ls <* skip
skip = many (satisfy isSpace)
