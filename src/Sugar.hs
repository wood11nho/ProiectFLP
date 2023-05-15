module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var v) = makeIndexedVar v

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar v i)
  | i == 0    = Var v
  | otherwise = Var (v ++ "_" ++ show i)

desugarExp :: ComplexExp -> Exp
desugarExp (CX v) = X (desugarVar v)
desugarExp (CLam v e) = Lam (desugarVar v) (desugarExp e)
desugarExp (CApp e1 e2) = App (desugarExp e1) (desugarExp e2)

sugarExp :: Exp -> ComplexExp
sugarExp (X v) = CX (sugarVar v)
sugarExp (Lam v e) = CLam (sugarVar v) (sugarExp e)
sugarExp (App e1 e2) = CApp (sugarExp e1) (sugarExp e2)