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

main :: IO ()
main = do
    content <- readFile "4.txt"
    let e = parseAll content
    let sleepyhead = fst $ sleepiestGuard e
        bestMin = fst $ sleepiestMinute (filter ((==sleepyhead) . guard) e)
    print (sleepyhead * bestMin)
    let bestGlobal = bestest snd $ fmap sleepiestMinute $ groupGuards e
    print (fst bestGlobal * fst (snd bestGlobal))

groupGuards = M.fromListWith (++) . map (\e -> (guard e, [e]))
range :: Event -> Int
range Event{..} = end-begin
bestest :: (Ord o) => (b -> o) -> M.Map a b -> (a,b)
bestest f = maximumBy (comparing (f.snd)) . M.toList
sleepiestGuard :: [Event] -> (Int, Int)
sleepiestGuard events =  bestest id . M.fromListWith (+) $  map (\e -> (guard e, range e)) events
sleepiestMinute :: [Event] -> (Int, Int)
sleepiestMinute = bestest id . M.fromListWith (+) . map (,1) . concatMap sploof
  where sploof Event{..} = [begin..end-1]

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
skip = many (satisfy isSpace)
data Event = Event {guard :: Int, begin :: Int, end :: Int}
  deriving Show
parseEvent :: Parser [Event]
parseEvent = do
    parseTimestamp
    string "Guard #"
    guard <- parseInt
    string "begins shift"
    many (satisfy isSpace)
    many $ try $ do
        begin <- parseTimestamp
        string "falls asleep"
        many (satisfy isSpace)
        end <- parseTimestamp
        string "wakes up"
        many (satisfy isSpace)
        return $ Event{..}
