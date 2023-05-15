module Main (main) where

import System.IO
import Data.String

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import Eval
import Sugar

main :: IO ()
main = do
    putStr "doardinamobucurestiHaskell> "
    hFlush stdout
    line <- getLine
    let parsed = parseFirst replCommand line
    case parsed of
        Just Quit -> return ()
        Just (Load s) -> do
            putStrLn ("Loading file " ++ s ++ "...")
            -- TODO: add code to load the file
            main
        Just (Eval s) -> do
            case parseFirst expr s of
                Just e -> do 
                    let e' = sugarExp (normalize (desugarExp e))
                    putStrLn (showExp e')
                    main
                Nothing -> do
                    putStrLn "Invalid expression"
                    main
        Nothing -> do
            putStrLn "Invalid command"
            main
