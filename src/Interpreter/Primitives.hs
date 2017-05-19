module Interpreter.Primitives (
  builtinPrefix,
  prelude
) where
import Interpreter.Defs
import Interpreter.Eval

wrap_2i_i :: String -> (Int -> Int -> Int) -> Primitive
wrap_2i_b :: String -> (Int -> Int -> Bool) -> Primitive
wrap_2b_b :: String -> (Bool -> Bool -> Bool) -> Primitive

builtinPrefix :: String



-- builtin prefix
builtinPrefix = "_bltn_"


-- primitive-related functions

wrap_2i_i n f = Prim2 n (TLambda TInt $ TLambda TInt TInt) $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DInt $ f e1 e2

wrap_2i_b n f = Prim2 n (TLambda TInt $ TLambda TInt TBool) $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DBool $ f e1 e2

wrap_2b_b n f = Prim2 n (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  DBool e2 <- exec e2
  return $ DBool $ f e1 e2


-- primitives

pAdd = wrap_2i_i "add" (+)
pSub = wrap_2i_i "sub" (-)
pMul = wrap_2i_i "mul" (*)
pDiv = wrap_2i_i "div" div
pMod = wrap_2i_i "mod" mod

pEq = wrap_2i_b "eq" (==)
pNeq = wrap_2i_b "neq" (/=)
pLt = wrap_2i_b "lt" (<)
pGt = wrap_2i_b "gt" (>)
pLe = wrap_2i_b "le" (<=)
pGe = wrap_2i_b "ge" (>=)

pAnd = Prim2 "and" (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then exec e2
  else return $ DBool False

pOr = Prim2 "or" (TLambda TBool $ TLambda TBool TBool) $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then return $ DBool True
    else exec e2

pNeg = Prim1 "neg" (TLambda TBool TBool) $ \e1 -> do
  DBool e1 <- exec e1
  return $ DBool $ not e1

pIf = Prim3 "if" (TLambda TBool $ TLambda (TVar "a") $ TLambda (TVar "a") (TVar "a")) $
  \e1 e2 e3 -> do
    DBool cond <- exec e1
    if cond
      then exec e2
      else exec e3


-- primitives aggregation
builtins :: [(Primitive, VarE)]
builtins = map (\p -> (p, builtinPrefix ++ takeName p))
  [pAdd, pSub, pMul, pDiv, pMod, pEq, pNeq, pLt, pGt, pLe, pGe, pAnd, pOr, pNeg, pIf]

prelude :: Exp -> Exp
prelude = ELet (Position "prelude" 0 0)
  (map (\(pr, n) -> (n, EData (Position "prelude" 0 0) $ DPrimitive pr)) builtins)
