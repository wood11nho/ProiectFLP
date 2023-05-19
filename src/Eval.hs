{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Eval where

import Exp
import Data.List (union, delete, nub)

vars :: Exp -> [IndexedVar]
vars (X v) = [v]
vars (Lam v e) = v : delete v (vars e)
vars (App e1 e2) = vars e1 `union` vars e2

freeVars :: Exp -> [IndexedVar]
freeVars (X v) = [v]
freeVars (Lam v e) = delete v (freeVars e)
freeVars (App e1 e2) = freeVars e1 `union` freeVars e2

occursFree :: IndexedVar -> Exp -> Bool
occursFree v e = v `elem` freeVars e

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar v vs
  | v `elem` vs = freshVar (v { ivCount = ivCount v + 1 }) vs
  | otherwise   = v

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar toReplace replacement (X v)
    | v == toReplace = X replacement
    | otherwise      = X v
renameVar toReplace replacement (Lam v e)
    | v == toReplace = Lam replacement (renameVar toReplace replacement e)
    | otherwise      = Lam v (renameVar toReplace replacement e)
renameVar toReplace replacement (App e1 e2) =
    App (renameVar toReplace replacement e1) (renameVar toReplace replacement e2)

substitute :: IndexedVar -> Exp -> Exp -> Exp
substitute toReplace replacement (X v)
    | v == toReplace = replacement
    | otherwise      = X v
substitute toReplace replacement (Lam v e)
    | v == toReplace = Lam v e
    | v `elem` freeVars replacement =
        let v' = freshVar v (vars replacement `union` vars e)
        in Lam v' (substitute toReplace replacement (renameVar v v' e))
    | otherwise      = Lam v (substitute toReplace replacement e)
substitute toReplace replacement (App e1 e2) =
    App (substitute toReplace replacement e1) (substitute toReplace replacement e2)

normalize :: Exp -> Exp
normalize (X v) = X v
normalize (Lam v e) = Lam v (normalize e)
normalize (App e1 e2) =
    case normalize e1 of
        Lam v e1' -> normalize (substitute v e2 e1')
        e1'       -> App e1' (normalize e2)