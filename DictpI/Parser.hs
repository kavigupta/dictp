module DictpI.Parser(DictpAST(..), dictP, parserTests) where

import Text.Parsec
import Data.Functor.Identity
import DictpI.Testing

data DictpAST = Symbol String
                | LiteralString String
                | LiteralDictionary [(DictpAST, DictpAST)]
                | TestContains DictpAST
                | Subscript DictpAST DictpAST
                | Set DictpAST DictpAST
                | Lambda [DictpAST]
        deriving (Eq, Show)

type Parser x = ParsecT String String Identity x

doParse :: Parser a -> String -> Either ParseError a
doParse parser input = runIdentity $ runParserT parser "(unknown)" "" input

nonSpacedDictP :: Parser DictpAST
nonSpacedDictP = (LiteralString <$> literalString)
            <|> (LiteralDictionary <$> literalDict)
            <|> try set
            <|> try subscript
            <|> testContains
            <|> lambda
            <|> (Symbol <$> symbol)

dictP :: Parser DictpAST
dictP = do
    spaces
    val <- nonSpacedDictP
    spaces
    return val

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

dictEntry :: Parser (DictpAST, DictpAST)
dictEntry = do
    key <- dictP
    spaces
    _ <- char ':'
    spaces
    value <- dictP
    spaces
    return (key, value)

literalDict :: Parser [(DictpAST, DictpAST)]
literalDict = do
        _ <- char '{'
        spaces
        entries <- many (try commaTerminatedEntry)
        lastEntry <- optionMaybe dictEntry
        case lastEntry of
            Nothing -> return []
            Just lastEntry' -> char '}' >> return (entries ++ [lastEntry'])

commaTerminatedEntry :: Parser (DictpAST, DictpAST)
commaTerminatedEntry = do
    u <- dictEntry
    spaces
    _ <- char ','
    return u

testContains :: Parser DictpAST
testContains = lookAhead $ do
    subscr <- subscript
    spaces
    _ <- char '?'
    return $ TestContains subscr

binaryGroup :: (Char, Char) -> (DictpAST -> DictpAST -> DictpAST) -> Parser DictpAST
binaryGroup (start, end) combinator = do
    spaces
    _ <- char start
    spaces
    dict <- dictP
    spaces
    key <- dictP
    spaces
    _ <- char end
    return $ combinator dict key

subscript :: Parser DictpAST
subscript = binaryGroup ('[', ']') Subscript

set :: Parser DictpAST
set = do
    spaces
    lhs <- subscript <|> fmap Symbol symbol
    spaces
    _ <- char '='
    spaces
    rhs <- dictP
    return $ Set lhs rhs

lambda :: Parser DictpAST
lambda = do
    spaces
    _ <- char '<'
    spaces
    body <- many dictP
    spaces
    _ <- char '>'
    spaces
    return $ Lambda body

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
        , assertEqual ("subscript symbol symbol", doParse subscript "[a b]",
                Right
                    (Subscript
                        (Symbol "a")
                        (Symbol "b")))
        , assertEqual ("subscript with strings", doParse subscript "[`a' `b']",
                Right
                    (Subscript
                        (LiteralString "a")
                        (LiteralString "b")))
        , assertEqual ("set expr", doParse set "[a `b'] = c",
                Right
                    (Set
                        (Subscript
                            (Symbol "a")
                            (LiteralString "b"))
                        (Symbol "c")))
        , assertEqual ("contains expr", doParse testContains "[a `b']?",
                Right
                    (TestContains
                        (Subscript
                            (Symbol "a")
                            (LiteralString "b"))))
        , assertEqual ("entry", doParse dictEntry "x : `2'",
            Right (Symbol "x", LiteralString "2"))
        , assertEqual ("empty dict", doParse literalDict "{}",
                Right [])
        , assertEqual ("one element dict", doParse literalDict "{x : `2'}",
            Right [(Symbol "x", LiteralString "2")])
        , assertEqual ("one element dict", doParse literalDict "{x : `2', x : a }",
            Right [
                (Symbol "x", LiteralString "2")
                , (Symbol "x",
                    Symbol "a")])
        , assertEqual ("one element dict", doParse literalDict "{x : `2', x : a = `b' }",
            Right [
                (Symbol "x", LiteralString "2")
                , (Symbol "x",
                    Set
                        (Symbol "a")
                        (LiteralString "b"))])
        , assertEqual ("one element lambda", doParse dictP "< [[+ x] x] >",
            Right $ Lambda [
                    Subscript
                        (Subscript
                            (Symbol "+")
                            (Symbol "x"))
                        (Symbol "x")
                ])
        , assertEqual ("multi element lambda", doParse dictP "< [print x] [[+ x] x] >",
            Right $ Lambda [
                    Subscript
                        (Symbol "print")
                        (Symbol "x"),
                    Subscript
                        (Subscript
                            (Symbol "+")
                            (Symbol "x"))
                        (Symbol "x")
                ])
    ]
