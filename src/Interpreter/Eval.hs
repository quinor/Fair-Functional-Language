{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}

module Interpreter.Eval (
  evalProgram,
  exec,
  computeData
) where
import Interpreter.Defs
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map

-- execute the expression (AST) and return the result
exec :: Namespace -> Stacktrace -> Exp -> Interpreter Data

-- ensure data is not lazied
computeData :: Stacktrace -> Data -> Interpreter Data

-- run expression and retrieve the result
evalProgram :: Exp -> Either (Stacktrace, String) Data

-- primitive applivation function
applyPrimitive :: Stacktrace -> Primitive -> Data -> Interpreter Data

applyPrimitive _ (Prim0 _ _ _) _                    = undefined --should never happen!
applyPrimitive st (Prim1 name (TLambda _ b) fn) ex  = return $ DPrimitive $ Prim0 name b $ fn ex st
applyPrimitive _ (Prim2 name (TLambda _ b) fn) ex   = return $ DPrimitive $ Prim1 name b $ fn ex
applyPrimitive _ (Prim3 name (TLambda _ b) fn) ex   = return $ DPrimitive $ Prim2 name b $ fn ex


-- exec helper functions for state monad

nextId :: MonadState (Sequence.Seq a) m => m Int
nextId = state (\st -> (length st, st))

addNext :: MonadState (Sequence.Seq a) m => a -> m ()
addNext dt = state (\st -> ((), st Sequence.|> dt))


-- computeData implementation
computeData st (DRef pos) = do
  state (\st -> (st `Sequence.index` pos, st)) >>= \case
    DLazy ns st ex  -> do
      state (\st -> ((), Sequence.update pos DUndefined st))
      dt_val <- exec ns st ex >>= computeData st
      state (\st -> ((), Sequence.update pos dt_val st))
      return dt_val
    -- only happens between lines 47..48
    DUndefined      -> throwError (st, "unsolvable infinite recursion!")
    x               -> computeData st x

computeData st (DPrimitive (Prim0 _ _ d)) = d >>= computeData st
computeData _ (DLazy ns st ex) = undefined --should never ever happen!
computeData _ a = return a


-- exec implementation
exec ns st ex = case ex of
  EVar pos var                        -> do
    return $ DRef $ fromJust $ Map.lookup var ns
  EData _ dt                          -> return dt
  EApply pos fun val                  -> do
    funLazy <- exec ns (pos:st) fun
    fun <- computeData st funLazy
    next <- nextId
    addNext (DLazy ns st val)
    case fun of
      DLambda f_ns var def  -> do
        exec (Map.insert var next f_ns) (pos:st) def
      DPrimitive prim       -> applyPrimitive st prim (DRef next)
      x                     -> undefined -- you can apply nothig else
  ELetRec pos nameDefs ex            -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _), place) m -> Map.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy new_ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELet pos nameDefs ex               -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _), place) m -> Map.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELambda pos var def                 -> do
    return $ DLambda ns var def

evalProgram prog = fst $ runState (runExceptT $ exec Map.empty [] prog >>= computeData []) Sequence.empty
