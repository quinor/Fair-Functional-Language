import Interpreter.Execute (run_program)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStr "No input file!\n"
    else run_program $ head args
