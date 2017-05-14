module Interpreter.Execute (
  run_program
) where
import Interpreter.Defs
import Interpreter.Parser
import Interpreter.Typecheck
import Interpreter.Eval
import Interpreter.Primitives
import Data.Either

--run program from file
run_program :: String -> IO ()

run_program fn = do
  f <- readFile fn
  let prog = parse_str f
  case prog of
    Left err    -> print err
    Right tlds  -> mapM_ (
      \(NamedExp n e) -> do
        let
          e' = prelude e
          t = check_type e'
          d = eval_program e'
        putStrLn $ n ++ " = " ++ (show d) ++ " :: " ++ (show t)
      ) tlds
