import Interpreter.Execute (runProgram)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  fn <- if null args
    then getContents
    else readFile $ head args
  runProgram fn
