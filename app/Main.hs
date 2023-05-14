module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand

main :: IO ()
main = do
    putStr ">>>"
    hFlush stdout
    line <- getLine
    let command = parseFirst replCommand line
    case command of
        Just Quit -> return ()
        Just (Load s) -> do
            putStrLn "Load"
            main
        Just (Eval s) -> do
            case parseExp s of
                Just exp -> do
                    putStrLn (showExp exp)
                    main
                Nothing -> do
                    putStrLn "Invalid expression!"
                    main
        Nothing -> do
            putStrLn "Invalid command!"
            main