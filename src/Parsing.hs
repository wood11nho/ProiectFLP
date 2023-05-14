
module Parsing where

import Exp
import Lab2
import Control.Applicative (some, many, (<|>))
import Data.Char (isAlpha, isAlphaNum)

parseFirst :: Parser a -> String -> Maybe a
parseFirst p s
  = case apply p s of
      [] -> Nothing
      (a,_):_ -> Just a

takeword :: String -> String
takeword "" = ""
takeword (' ' : xs) = ""
takeword (x : xs) = x : takeword xs

letter :: Parser Char
letter = satisfy isAlpha


letternum :: Parser Char
letternum = satisfy isAlphaNum

var :: Parser Var
var = fmap Var (identifier letter letternum)
-- >>> parseFirst var "b is a var"
-- Just (Var {getVar = "b"})

varExp :: Parser ComplexExp
varExp = fmap CX var
-- >>> parseFirst varExp "b is a var"
-- Just (CX (Var {getVar = "b"}))

findSlash :: Parser ()
findSlash = reserved "\\"

findArrow :: Parser ()
findArrow = reserved "->"

lambdaExp :: Parser ComplexExp
lambdaExp = do
              findSlash
              var <- var
              findArrow
              exp <- expr
              return (CLam var exp)

-- >>> parseFirst lambdaExp "\\x -> x"
-- Just (CLam (Var {getVar = "x"}) (CX (Var {getVar = "x"})))

parenExp :: Parser ComplexExp
parenExp = do
            reserved "("
            exp <- expr
            reserved ")"
            return exp
-- >>> parseFirst parenExp "(a)"
-- Just (CX (Var {getVar = "a"}))

basicExp :: Parser ComplexExp
basicExp = lambdaExp <|> varExp <|> parenExp
-- >>> parseFirst basicExp "[a,b,c]"r
-- Just (List [CX (Var {getVar = "a"}),CX (Var {getVar = "b"}),CX (Var {getVar = "c"})])

appExp :: Parser ComplexExp
appExp = do
            exp1 <- basicExp
            exp2 <- basicExp
            rest <- many basicExp
            return (foldl CApp (CApp exp1 exp2) rest) 

expr :: Parser ComplexExp
expr = basicExp <|> appExp
-- >>> parseFirst expr "\\x -> x y z t"
-- Just (CLam (Var {getVar = "x"}) (CApp (CApp (CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y"}))) (CX (Var {getVar = "z"}))) (CX (Var {getVar = "t"}))))

exprParser :: Parser ComplexExp
exprParser = whiteSpace *> expr <* endOfInput
-- >>> parseFirst exprParser "let x := 28 in \\y -> + x y"
-- Just (Let (Var {getVar = "x"}) (Nat 28) (CLam (Var {getVar = "y"}) (CApp (CApp (CX (Var {getVar = "+"})) (CX (Var {getVar = "x"}))) (CX (Var {getVar = "y"})))))

