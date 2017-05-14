module Interpreter.Primitives (
  builtin_prefix,
  prelude
) where
import Interpreter.Defs
import Interpreter.Eval

wrap_2i_i :: (Int -> Int -> Int) -> Primitive
wrap_2i_b :: (Int -> Int -> Bool) -> Primitive
wrap_2b_b :: (Bool -> Bool -> Bool) -> Primitive

builtin_prefix :: String



-- builtin prefix
builtin_prefix = "__builtin__"


-- primitive-related functions

wrap_2i_i f = Prim2 (TLambda TInt $ TLambda TInt TInt) $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DInt $ f e1 e2

wrap_2i_b f = Prim2 (TLambda TInt $ TLambda TInt TBool) $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DBool $ f e1 e2

wrap_2b_b f = Prim2 (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  DBool e2 <- exec e2
  return $ DBool $ f e1 e2


-- primitives

p_add = wrap_2i_i (+)
p_sub = wrap_2i_i (-)
p_mul = wrap_2i_i (*)
p_div = wrap_2i_i div
p_mod = wrap_2i_i mod

p_eq = wrap_2i_b (==)
p_neq = wrap_2i_b (/=)
p_lt = wrap_2i_b (<)
p_gt = wrap_2i_b (>)
p_le = wrap_2i_b (<=)
p_ge = wrap_2i_b (>=)

p_and = Prim2 (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then exec e2
  else return $ DBool False

p_or = Prim2 (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then return $ DBool True
    else exec e2

p_neg = Prim1 (TLambda TBool TBool) $ \e1 -> do
  DBool e1 <- exec e1
  return $ DBool $ not e1

p_if = Prim3 (TLambda TBool $ TLambda (TVar "a") $ TLambda (TVar "a") (TVar "a")) $
  \e1 e2 e3 -> do
    DBool cond <- exec e1
    if cond
      then exec e2
      else exec e3


-- primitives aggregation
builtins :: [(Primitive, VarE)]
builtins = [
  (p_add, builtin_prefix ++ "add"),
  (p_sub, builtin_prefix ++ "sub"),
  (p_mul, builtin_prefix ++ "mul"),
  (p_div, builtin_prefix ++ "div"),
  (p_mod, builtin_prefix ++ "mod"),
  (p_eq, builtin_prefix ++ "eq"),
  (p_neq, builtin_prefix ++ "neq"),
  (p_lt, builtin_prefix ++ "lt"),
  (p_gt, builtin_prefix ++ "gt"),
  (p_le, builtin_prefix ++ "le"),
  (p_ge, builtin_prefix ++ "ge"),
  (p_and, builtin_prefix ++ "and"),
  (p_or, builtin_prefix ++ "or"),
  (p_neg, builtin_prefix ++ "neg"),
  (p_if, builtin_prefix ++ "if")]

prelude :: Exp -> Exp
prelude ex = foldr
  (\(p, n) e -> ELet n (EData $ DPrimitive p) e)
  ex
  builtins

