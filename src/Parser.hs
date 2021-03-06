
module Parser (
        module Parser,
        module Control.Applicative
        ) where

import Control.Monad.State
import Control.Applicative
import Data.Char (isAlpha, isSpace)

type Parser = StateT String Maybe

item :: Parser Char
item = get >>= \s -> case s of
                         [] -> empty
                         (x:xs) -> put xs >> return x

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \i -> guard (p i) >> return i

char :: Char -> Parser Char
char = sat . (==)

string :: String -> Parser String
string = mapM char

skip :: (Char -> Bool) -> Parser ()
skip p = void . many $ sat p

oneOf :: String -> Parser Char
oneOf = foldr (<|>) empty . map char

spaces :: Parser ()
spaces = skip isSpace

reserve :: [String]
reserve = ["let", "in", "fix"]

token :: Parser String
token = do
        x <- spaces *> (some $ sat isAlpha) <* spaces
        guard $ x `notElem` reserve
        return x

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p op = p >>= chain where
    chain l = foldl (\a (f, b) -> f a b) l <$> many ((,) <$> op <*> p)

integer :: Parser Int
integer = do
        s <- string "-" <|> pure ""
        i <- (:) <$> oneOf ['1'..'9'] <*> many (oneOf ['0'..'9'])
        return (read $ s ++ i)

parse :: Parser a -> String -> Maybe a
parse parser str = do
        (l, s) <- runStateT parser str
        guard $ all isSpace s
        return l
