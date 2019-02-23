module Foo where
import Text.Megaparsec as P
import Text.Megaparsec.Char
import Data.Void
import Control.Monad (replicateM)
import Data.Function
import Data.Char
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V

part1, part2 :: AST -> Int
part1 = cata ((+) `on` sum)
part2 = cata step
  where
    step children meta
      | null children = sum meta
      | otherwise = sum $ backpermute children meta

-- with unboxed vectors we can't accidentally keep a reference to vals by being
-- insufficiently strict
backpermute :: (Num a, V.Unbox a) => [a] -> [Int] -> [a]
backpermute vals = map getIndexAt
  where
    getIndexAt = fromMaybe 0 . (vals' V.!?) . pred
    vals' = V.fromList vals

main = do
    input <- T.readFile "8.txt"
    let parsed = parseAll input
    print (part1 parsed)
    print (part2 parsed)

data AST = AST [AST] [Int]

cata :: ([a] -> [Int] -> a) -> AST -> a
cata f = go
  where go (AST ast m) = f (fmap go ast) m

pAST :: Parser AST
pAST = do
    countChildren <- pInt
    countMeta <- pInt
    children <- replicateM countChildren pAST
    meta <- replicateM countMeta pMeta
    pure (AST children meta)
pMeta = pInt

parseAll :: T.Text -> AST
parseAll ls = case runParser pAST "" ls  of
    Right x -> x
    Left err -> error (parseErrorPretty err)
type Parser = Parsec Void T.Text
pInt :: Parser Int
pInt = lexeme $ toInt <$> takeWhile1P Nothing isDigit
  where
    toInt = T.foldl step 0
    step acc cur = digitToInt cur + 10 * acc
    
lexeme p = p <* skip
skip :: Parser ()
skip = dropWhile1P (Just "Spaces") isSpace
