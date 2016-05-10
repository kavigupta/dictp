module DictpI.Testing(
        assertError, assertEqual
    ) where

assertError :: (String, Either a b) -> (String, Bool)
assertError (str, Left _) = (str, True)
assertError (str, Right _) = (str, False)

assertEqual :: (Eq a, Show a) => (String, a, a) -> (String, Bool)
assertEqual (msg, x, y)
    | x == y    = (msg, True)
    | otherwise = (msg ++ "\t" ++ show x ++ " /= " ++ show y, False)