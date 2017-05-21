{-# Options -Wall #-}

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
import qualified Data.Map as M

-- execute the expression (AST) and return the result
exec :: Namespace -> Stacktrace -> Exp -> Interpreter Data

-- ensure data is not lazied
computeData :: Stacktrace -> Data -> Interpreter Data

-- run expression and retrieve the result
evalProgram :: Exp -> Either (Stacktrace, String) Data

-- primitive applivation function
applyPrimitive :: Stacktrace -> Primitive -> Data -> Interpreter Data

applyPrimitive _ (Prim0 _ _ _) _                    = undefined --should never happen!
applyPrimitive _ (Prim1 name (TLambda _ b) fn) ex  = return $ DPrimitive $ Prim0 name b $ fn ex
applyPrimitive _ (Prim2 name (TLambda _ b) fn) ex   = return $ DPrimitive $ Prim1 name b $ fn ex
applyPrimitive _ (Prim3 name (TLambda _ b) fn) ex   = return $ DPrimitive $ Prim2 name b $ fn ex
applyPrimitive _ _ _                                = undefined


-- exec helper functions for state monad

nextId :: MonadState (Sequence.Seq a) m => m Int
nextId = state (\st -> (length st, st))

addNext :: MonadState (Sequence.Seq a) m => a -> m ()
addNext dt = state (\st -> ((), st Sequence.|> dt))


-- computeData implementation
computeData st (DRef pos) = do
  state (\sta -> (sta `Sequence.index` pos, sta)) >>= \case
    DLazy ns st' ex  -> do
      state (\sta -> ((), Sequence.update pos DUndefined sta))
      dt_val <- exec ns st' ex >>= computeData st'
      state (\sta -> ((), Sequence.update pos dt_val sta))
      return dt_val
    -- only happens between lines 47..48
    DUndefined      -> throwError (st, "unsolvable infinite recursion!")
    x               -> computeData st x

computeData st (DPrimitive (Prim0 _ _ d)) = (d st) >>= computeData st
computeData _ (DLazy _ _ _) = undefined --should never ever happen!
computeData _ a = return a


-- exec implementation
exec ns st expr = case expr of
  EVar _ var                          -> do
    return $ DRef $ fromJust $ M.lookup var ns
  EData _ dt                          -> return dt
  EApply pos fun val                  -> do
    funLazy <- exec ns (pos:st) fun
    funVal <- computeData st funLazy
    next <- nextId
    addNext (DLazy ns st val)
    case funVal of
      DLambda f_ns var def  -> do
        exec (M.insert var next f_ns) (pos:st) def
      DPrimitive prim       -> applyPrimitive st prim (DRef next)
      _                     -> undefined -- you can apply nothig else
  ELetRec pos nameDefs ex             -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _), place) m -> M.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy new_ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELet pos nameDefs ex                -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _), place) m -> M.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELambda _ var def                   -> do
    return $ DLambda ns var def

evalProgram prog = fst $ runState (runExceptT $ exec M.empty [] prog >>= computeData []) Sequence.empty
