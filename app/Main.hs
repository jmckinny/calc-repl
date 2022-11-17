module Main where
import Braces
import Parser
import Control.Monad
import System.IO


main :: IO  ()
main = do
    putStr "> "
    hFlush stdout
    done <- isEOF
    if not done then do
        input <- getLine
        if validBraces input then
            unless (input == "quit" || input == "q")
                $ print (parseEval input) >> main
        else putStrLn "Invalid Braces!" >> main
    else putStrLn "Bye!"
