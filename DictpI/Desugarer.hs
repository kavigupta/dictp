module DictpI.Desugarer (
        DictPDesugared(..), desugar
    ) where

import qualified DictpI.Parser as P

data PrimitiveValue = Empty | Locals | LiteralString String

data DictPDesugared
    = Primitive PrimitiveValue
        | Contains DictPDesugared DictPDesugared
        | Subscript DictPDesugared DictPDesugared
        | Set PrimitiveValue PrimitiveValue PrimitiveValue
        | Sequence [PrimitiveValue]
        | Lambda PrimitiveValue

desugar :: P.DictpAST -> DictPDesugared
desugar (P.Symbol string) = Subscript (Primitive Locals) (Primitive $ LiteralString string)
desugar (P.LiteralString string) = Primitive $ LiteralString string
desugar (P.LiteralDictionary values) = desugarDictionary values


desugarDictionary :: [(P.DictpAST, P.DictpAST)] -> [DictPDesugared]
desugarDictionary [] = Empty
desugarDictionary (x:xs) =