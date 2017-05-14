import Interpreter.Execute (run_program)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  fn <- if null args
    then getContents
    else readFile $ head args
  run_program fn
