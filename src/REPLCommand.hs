module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

--Acest parser va trebui să înțeleagă următoarele comenzi:

-- :q sau :quit pentru Quit
-- :l sau :load, urmate de un șir de caractere pentru Load
-- dacă nu e nici unul din cazurile de mai sus, tot șirul de intrare va fi pus într-un Eval.

instance Show REPLCommand where
  show Quit = "Quit"
  show (Load s) = "Load " ++ s
  show (Eval s) = "Eval " ++ s

quit :: Parser REPLCommand
quit = do
  reserved ":q"
  return Quit
  <|> do
  reserved ":quit"
  return Quit

load :: Parser REPLCommand
load = do
  reserved ":l"
  s <- (many anychar)
  return (Load s)
  <|> do
  reserved ":load"
  s <- many anychar
  return (Load s)

eval :: Parser REPLCommand
eval = do
  s <- many anychar
  return (Eval s)


replCommand :: Parser REPLCommand
replCommand = quit <|> load <|> eval