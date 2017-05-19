module Main where
import Interpreter.Execute (runProgram)
import System.Environment
import Control.Monad

main :: IO ()
main = do
  args <- getArgs
  (fn, contents) <- (uncurry $ liftM2 (,)) $ if null args
    then (return "<stdin>", getContents)
    else (return $ head args, readFile $ head args)
  runProgram fn contents
