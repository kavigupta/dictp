module DictpI.Tests(tests) where

import DictpI.Parser
import Distribution.TestSuite

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
tests = return $ map suiteify parserTests

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