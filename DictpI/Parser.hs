module DictpI.Parser(DictpAST(..), dictP, parserTests) where

import Text.Parsec
import Data.Functor.Identity
import DictpI.Testing

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
symbol = many1 (alphaNum <|> oneOf "-_~!@#$%^&*+")

-- Use ` and ' to quote a string
literalString :: Parser String
literalString = do
        _ <- char '`'
        value <- strContents
        _ <- char '\''
        return value

quotedString :: Parser String
quotedString = do
    str <- literalString
    return $ "`" ++ str ++  "'"

strContents :: Parser String
strContents = do
        chunk <- fmap Just (innocuous <|> quotedString) <|> fmap (const Nothing) (lookAhead $ char '\'')
        case chunk of
            Nothing -> return ""
            Just chunk' -> do
                rest <- strContents
                return $ chunk' ++ rest
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

parserTests :: [(String, Bool)]
parserTests = [
        assertEqual ("parse symbol", doParse symbol "abc* ", Right "abc*")
        , assertEqual ("parse other symbol", doParse symbol "*", Right "*")
        , assertEqual ("parse name", doParse symbol "variable_name", Right "variable_name")
        , assertEqual ("parse multiple", doParse symbol "variable_name asdf", Right "variable_name")
        , assertError ("parse failure", doParse symbol "")
        , assertEqual ("empty string contents", doParse strContents "'", Right "")
        , assertEqual ("parse empty string", doParse literalString "`'", Right "")
        , assertEqual ("parse string with stuff in it", doParse literalString "`abc'", Right "abc")
        , assertEqual ("parse string with whitespace, double quotes", doParse literalString "`abc   \"\"\" '", Right "abc   \"\"\" ")
        , assertEqual ("parse string with whitespace, double quotes", doParse literalString "`The word is `abc''", Right "The word is `abc'")
    ]
