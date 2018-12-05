{-# LANGUAGE RecordWildCards #-}
{-# Language TupleSections #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
import qualified Data.Map as M
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Data.Void
import Data.Char

main = do
    content <- readFile "3.txt"
    let rects = getRect content
    let spots = M.fromListWith (+) $ concatMap (map (,1) . dots) rects
    print $ length $ filter (>1) $ M.elems spots
    let check rect = and [spots M.! p == 1| p <- dots rect]
    print $ filter check rects

dots :: Rect -> [(Int,Int)]
dots Rect{..} = [(x+w,y+h) | w <- [0..width-1], h <- [0..height-1] ]

getRect :: String -> [Rect]
getRect ls = case runParser (parseLine `sepEndBy` newline) "" ls  of
    Right x -> x
    Left err -> error (parseErrorPretty err)
parseInt :: Parser Int
parseInt = read <$> takeWhile1P Nothing isDigit
data Rect = Rect { id:: Int, x::Int, y::Int, width:: Int, height::Int }
  deriving (Show, Eq, Ord)
type Parser = Parsec Void String

parseLine :: Parser Rect
parseLine = do
    char' '#'
    id <- parseInt
    string " @ "
    x <- parseInt
    char' ','
    y <- parseInt
    string ": "
    width <- parseInt
    char' 'x'
    height <- parseInt
    return Rect{..}



