{-# Options -Wall #-}

module Interpreter.Primitives (
  builtinPrefix,
  prelude
) where
import Interpreter.Defs
import Interpreter.Eval
import Control.Monad.Except

wrap_2i_i :: String -> (Int -> Int -> Int) -> Primitive
wrap_2i_b :: String -> (Int -> Int -> Bool) -> Primitive

builtinPrefix :: String



-- builtin prefix
builtinPrefix = "_bltn_"


-- primitive-related functions

wrap_2i_i n f = Prim n (TLambda TInt $ TLambda TInt TInt) 2 $ \[d1,d2] st -> do
  DInt d1' <- computeData st d1
  DInt d2' <- computeData st d2
  return $ DInt $ f d1' d2'

wrap_2i_b n f = Prim n (TLambda TInt $ TLambda TInt TBool) 2 $ \[d1,d2] st -> do
  DInt d1' <- computeData st d1
  DInt d2' <- computeData st d2
  return $ DBool $ f d1' d2'

{-
wrap_2b_b :: String -> (Bool -> Bool -> Bool) -> Primitive

-- maybe will be needed in the future though I doubt it, those are usually lazy
wrap_2b_b n f = Prim n (TLambda TBool $ TLambda TBool TBool) 2 $ \[d1,d2] st -> do
  DBool d1' <- computeData st d1
  DBool d2' <- computeData st d2
  return $ DBool $ f d1' d2'
-}


-- primitives

pAdd :: Primitive
pSub :: Primitive
pMul :: Primitive
pDiv :: Primitive
pMod :: Primitive
pEq  :: Primitive
pNeq :: Primitive
pLt  :: Primitive
pGt  :: Primitive
pLe  :: Primitive
pGe  :: Primitive
pAnd :: Primitive
pOr  :: Primitive
pNot :: Primitive
pIf  :: Primitive
pUndef :: Primitive

pAdd = wrap_2i_i "add" (+)
pSub = wrap_2i_i "sub" (-)
pMul = wrap_2i_i "mul" (*)

pEq = wrap_2i_b "eq" (==)
pNeq = wrap_2i_b "neq" (/=)
pLt = wrap_2i_b "lt" (<)
pGt = wrap_2i_b "gt" (>)
pLe = wrap_2i_b "le" (<=)
pGe = wrap_2i_b "ge" (>=)



pDiv = Prim "div" (TLambda TInt $ TLambda TInt TInt) 2 $ \[d1,d2] st -> do
  DInt d1' <- computeData st d1
  DInt d2' <- computeData st d2
  if d2' == 0
    then throwError (st, "zero division error!")
    else return $ DInt $ d1' `div` d2'

pMod = Prim "mod" (TLambda TInt $ TLambda TInt TInt) 2 $ \[d1,d2] st -> do
  DInt d1' <- computeData st d1
  DInt d2' <- computeData st d2
  if d2' == 0
    then throwError (st, "zero division error!")
    else return $ DInt $ d1' `mod` d2'



pAnd = Prim "and" (TLambda TBool $ TLambda TBool TBool) 2 $ \[d1,d2] st -> do
  DBool d1' <- computeData st d1
  if d1'
    then computeData st d2
  else return $ DBool False

pOr = Prim "or" (TLambda TBool $ TLambda TBool TBool) 2 $ \[d1,d2] st -> do
  DBool d1' <- computeData st d1
  if d1'
    then return $ DBool True
    else computeData st d2

pNot = Prim "not" (TLambda TBool TBool) 1 $ \[d1] st -> do
  DBool d1' <- computeData st d1
  return $ DBool $ not d1'

pIf = Prim "if" (TLambda TBool $ TLambda (TVar "a") $ TLambda (TVar "a") (TVar "a")) 3 $
  \[d1,d2,d3] st -> do
    DBool cond <- computeData st d1
    if cond
      then computeData st d2
      else computeData st d3

pUndef = Prim "undefined" (TVar "a") 0 $ \[] st -> throwError (st, "undefined expresison reached!")

-- primitives aggregation, first builtin ones, then named ones
builtins :: [(Primitive, VarE)]
builtins = (map (\p -> (p, builtinPrefix ++ takeName p))
  [pAdd, pSub, pMul, pDiv, pMod, pEq, pNeq, pLt, pGt, pLe, pGe, pAnd, pOr, pIf])
   ++ (map (\p -> (p, takeName p)) [pNot, pUndef])

prelude :: Exp -> Exp
prelude = ELet (Position "prelude" 0 0)
  (map (\(pr, n) -> (n, Nothing, EData (Position "prelude" 0 0) $ DPrimitive pr)) builtins)
