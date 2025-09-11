{-# LANGUAGE DataKinds,TypeFamilies,UndecidableInstances #-}
import GHC.TypeLits
type SymbolToCharList :: Symbol -> [Char]
type family SymbolToCharList s where 
    SymbolToCharList s = SymbolToCharList' (UnconsSymbol s)
type SymbolToCharList' :: Maybe (Char,Symbol) -> [Char]
type family SymbolToCharList' s where 
    SymbolToCharList' 'Nothing = '[]
    SymbolToCharList' ('Just '(c,s)) = c ': SymbolToCharList s
type family CharListToSymbol a where 
    CharListToSymbol '[] = ""
    CharListToSymbol (c ': cs) = ConsSymbol c (CharListToSymbol cs)


