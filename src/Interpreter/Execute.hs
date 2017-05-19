module Interpreter.Execute (
  runProgram
) where
import Interpreter.Defs
import Interpreter.Parser
import Interpreter.Typecheck
import Interpreter.Eval
import Interpreter.Primitives
import Data.Either
import Text.Megaparsec

--run program from file
runProgram :: String -> String -> IO ()

runProgram fn f = do
  let prog = parseStr fn f
  case prog of
    Left err    -> putStrLn $ parseErrorPretty err
    Right tlds  -> mapM_ (
      \(NamedExp n e) -> do
        let
          e' = prelude e
        putStrLn $ "processing entry " ++ show n ++ ":"
        putStrLn $ "code: \n" ++ show e
        case checkType e' of
          Left (pos, e)  -> do
           putStrLn "Typecheck failure:"
           putStrLn $ (show pos) ++ ": " ++ e
          Right t -> do
            putStrLn $ "type: " ++ show t
            case evalProgram e' of
              Left (st, e)  -> do
                putStrLn $ "Eval failure: " ++ e
                putStr $ printStacktrace st
              Right d -> putStrLn $ "result: " ++ show d
        putStrLn ""
      ) tlds
