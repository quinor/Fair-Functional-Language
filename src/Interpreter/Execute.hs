{-# Options -Wall #-}

module Interpreter.Execute (
  runProgram
) where
import Interpreter.Defs
import Interpreter.Parser
import Interpreter.Typecheck
import Interpreter.Eval
import Interpreter.Primitives
import Text.Megaparsec

--run program from file
runProgram :: String -> String -> IO ()

runProgram fn f = do
  let prog = parseStr fn f
  case prog of
    Left err    -> putStrLn $ parseErrorPretty err
    Right tlds  -> mapM_ (
      \(NamedExp n _ e) -> do
        let
          e' = prelude e
        putStrLn $ "processing entry " ++ show n ++ ":"
        putStrLn $ "code: \n" ++ show e
        case checkType e' of
          Left (pos, err)  -> do
           putStrLn "Typecheck failure:"
           putStrLn $ (show pos) ++ ": " ++ err
          Right t -> do
            putStrLn $ "type: " ++ show t
            case evalProgram e' of
              Left (st, err)  -> do
                putStrLn $ "Eval failure: " ++ err
                putStr $ printStacktrace st
              Right d -> putStrLn $ "result: " ++ show d
        putStrLn ""
      ) tlds
