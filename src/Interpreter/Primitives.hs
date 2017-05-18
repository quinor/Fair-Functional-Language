module Interpreter.Primitives (
  builtinPrefix,
  prelude
) where
import Interpreter.Defs
import Interpreter.Eval

wrap_2i_i :: (Int -> Int -> Int) -> Primitive
wrap_2i_b :: (Int -> Int -> Bool) -> Primitive
wrap_2b_b :: (Bool -> Bool -> Bool) -> Primitive

builtinPrefix :: String



-- builtin prefix
builtinPrefix = "__builtin__"


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

pAdd = wrap_2i_i (+)
pSub = wrap_2i_i (-)
pMul = wrap_2i_i (*)
pDiv = wrap_2i_i div
pMod = wrap_2i_i mod

pEq = wrap_2i_b (==)
pNeq = wrap_2i_b (/=)
pLt = wrap_2i_b (<)
pGt = wrap_2i_b (>)
pLe = wrap_2i_b (<=)
pGe = wrap_2i_b (>=)

pAnd = Prim2 (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then exec e2
  else return $ DBool False

pOr = Prim2 (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then return $ DBool True
    else exec e2

pNeg = Prim1 (TLambda TBool TBool) $ \e1 -> do
  DBool e1 <- exec e1
  return $ DBool $ not e1

pIf = Prim3 (TLambda TBool $ TLambda (TVar "a") $ TLambda (TVar "a") (TVar "a")) $
  \e1 e2 e3 -> do
    DBool cond <- exec e1
    if cond
      then exec e2
      else exec e3


-- primitives aggregation
builtins :: [(Primitive, VarE)]
builtins = [
  (pAdd, builtinPrefix ++ "add"),
  (pSub, builtinPrefix ++ "sub"),
  (pMul, builtinPrefix ++ "mul"),
  (pDiv, builtinPrefix ++ "div"),
  (pMod, builtinPrefix ++ "mod"),
  (pEq, builtinPrefix ++ "eq"),
  (pNeq, builtinPrefix ++ "neq"),
  (pLt, builtinPrefix ++ "lt"),
  (pGt, builtinPrefix ++ "gt"),
  (pLe, builtinPrefix ++ "le"),
  (pGe, builtinPrefix ++ "ge"),
  (pAnd, builtinPrefix ++ "and"),
  (pOr, builtinPrefix ++ "or"),
  (pNeg, builtinPrefix ++ "neg"),
  (pIf, builtinPrefix ++ "if")]

prelude :: Exp -> Exp
prelude ex = foldr
  (\(p, n) e -> ELet [(n, EData $ DPrimitive p)] e)
  ex
  builtins

