module Debug where
import Direct

debugName :: Term -> String
debugName (Lit lit) = show lit
debugName (Var x) = show x
debugName Abs{} = "Abs"
debugName App {} = "App"
debugName Let {} = "Let"
debugName Case {} = "Case"
debugName If {} = "If"
debugName Tuple{} = "Tuple"
debugName List{} = "List"
debugName ListCons{} = "ListCons"
debugName Bin{} = "Bin"
