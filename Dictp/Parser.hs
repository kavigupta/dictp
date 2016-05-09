module Dictp.Parser(DictpAST(..), dictP, doParse) where

import Text.Parsec
import Data.Functor.Identity

data DictpAST = Symbol String
                | LiteralString String
                | LiteralDictionary [(DictpAST, DictpAST)]
                | TestContains DictpAST DictpAST
                | Get DictpAST DictpAST
                | Set DictpAST DictpAST
                | Lambda [DictpAST]

type Parser x = ParsecT String String Identity x

doParse :: Parser a -> String -> Either ParseError a
doParse parser input = runIdentity $ runParserT parser "(unknown)" "" input

dictP :: Parser DictpAST
dictP = try (Symbol <$> symbol)
            <|> try (LiteralString <$> literalString)
            <|> try (LiteralDictionary <$> literalDict)
            <|> try testContains
            <|> try get
            <|> try set
            <|> try lambda

-- lots of possible symbols
symbol :: Parser String
symbol = many (alphaNum <|> oneOf "-_~!@#$%^&*+")

-- Use ` and ' to quote a string
literalString :: Parser String
literalString = do
        _ <- char '`'
        value <- strContents
        _ <- char '\''
        return value
    where
    strContents = do
            chunk <- innocuous <|> literalString
            rest <- strContents
            return $ chunk ++ rest
        where
        innocuous = do
            c <- noneOf "`'"
            return [c]

literalDict :: Parser [(DictpAST, DictpAST)]
literalDict = do
        _ <- char '{'
        spaces
        return undefined

testContains :: a
testContains = undefined
get :: a
get = undefined
set :: a
set = undefined
lambda :: a
lambda = undefined
