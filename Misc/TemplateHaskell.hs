module TemplateHaskell where
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
-- prettyPrint :: Exp -> String
-- prettyPrint (AppE f a) = prettyPrint f ++ " " ++ prettyPrint a
-- prettyPrint (VarE name) = nameBase name
-- prettyPrint (LitE lit) = show lit
-- prettyPrint (ConE name) = nameBase name
-- prettyPrint (LamE args body) = "\\" ++ unwords (map (nameBase . varName) args) ++ " = " ++ prettyPrint body
-- prettyPrint (LetE bindings body) = "let " ++ unwords (map (prettyPrint . snd) bindings) ++ " in " ++ prettyPrint body
-- prettyPrint _ = "<unsupported expression>"
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)
makelens :: Name -> Q [Dec]
makelens name = do
    d <- reifyDatatype name
    let c = datatypeCons d
        strname = nameBase name
    if length c /= 1
        then fail "Only one constructor allowed for lens generation."
        else do 
            let con = head c 
            let constructype = constructorVariant 
            
            case constructype con of
                RecordConstructor xs -> do
                    runIO $ putStrLn "Record constructor found."
                    --let myfun str = ValD (VarP (mkName str)) 
                    --                (NormalB (AppE (AppE (VarE 'lens) (ConE name))
                    return []
                _ -> fail "Only record constructors are supported."

x :: Q [Dec]
x = [d| myvar = 2|]
