module Interpreter.Execute (
  runProgram
) where
import Interpreter.Defs
import Interpreter.Parser
import Interpreter.Typecheck
import Interpreter.Eval
import Interpreter.Primitives
import Data.Either

--run program from file
runProgram :: String -> IO ()

runProgram f = do
  let prog = parseStr f
  case prog of
    Left err    -> print err
    Right tlds  -> mapM_ (
      \(NamedExp n e) -> do
        let
          e' = prelude e
          t = checkType e'
          d = evalProgram e'
        putStrLn $ n ++ " = " ++ show d ++ " :: " ++ show t
      ) tlds
