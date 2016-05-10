module DictpI.ParserTest(tests) where

import DictpI.Parser
import Distribution.TestSuite

theTests :: [(String, Bool)]
theTests = [
     assertEqual ("parse String", doParse symbol "abc* ", Right "abc*")
    ]

{-
    runParseTests :: [(String, Bool)] -> String
    runParseTests vals = if null failures
            then "Passed " ++ show (length vals) ++ " tests"
            else intercalate "\n" . map ("Failed test " ++) $ failures
        where
        failures :: [String]
        failures = map fst . filter (not . snd) $ vals
-}

tests :: IO [Test]
tests = return $ map suiteify theTests

assertEqual :: (Eq a, Show a) => (String, a, a) -> (String, Bool)
assertEqual (msg, x, y)
    | x == y    = (msg, True)
    | otherwise = (msg ++ "\t" ++ show x ++ " /= " ++ show y, False)

suiteify :: (String, Bool) -> Test
suiteify (msg, test) = Test testinst
    where
    testinst :: TestInstance
    testinst = TestInstance {
            run = return $ if test then Finished Pass else Finished (Fail msg),
            name = msg,
            tags = [],
            options = [],
            setOption = \_ _ -> Right testinst
        }