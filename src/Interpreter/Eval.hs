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

-- remove all references from data together with unlazying it, only for program output purposes
derefData :: Stacktrace -> Data -> Interpreter Data

-- run expression and retrieve the result
evalProgram :: Exp -> Either (Stacktrace, String) Data

-- exec helper functions for state monad

nextId :: MonadState (Sequence.Seq a) m => m Int
nextId = state (\st -> (length st, st))

addNext :: MonadState (Sequence.Seq a) m => a -> m ()
addNext dt = state (\st -> ((), st Sequence.|> dt))


-- computedata implementation
computeData st (DRef pos) = do
  value <- state (\sta -> (sta `Sequence.index` pos, sta))
  state (\sta -> ((), Sequence.update pos DUndefined sta))
  reduced <- computeData st value
  state (\sta -> ((), Sequence.update pos reduced sta))
  return reduced

computeData st (DPrimitive (Prim _ _ 0 d)) = (d [] st) >>= computeData st

--only happens below
computeData st (DUndefined) = throwError (st, "unsolvable infinite recursion!")

computeData st (DLazyEval ns st' ex) = exec ns st' ex >>= computeData st

computeData st (DLazyApply st' funL valL) = do
  fun <- computeData st' funL
  computeData st =<< case fun of
    DPrimitive (Prim _ _ 0 _)                   -> undefined -- should never happen, safeguard
    DPrimitive (Prim name (TLambda _ b) num fn) ->
      return $ DPrimitive $ Prim name b (num-1) (\l -> fn (valL:l))
    DLambda ns var def                          -> do
      next <- nextId
      addNext valL
      exec (M.insert var next ns) st' def
    _                                           -> undefined -- nothing more can be applied

computeData _ a = return a


-- compute data and resolve all references
derefData st d = computeData st d >>= \case
  DRef pos        -> derefData st =<< state (\sta -> (sta `Sequence.index` pos, sta))
  DAlgebraic n ds -> do
    ds' <- mapM (derefData st) ds
    return $ DAlgebraic n ds'
  x               -> return x


-- exec implementation
exec ns st expr = case expr of
  EVar _ var                          -> do
    return $ DRef $ fromJust $ M.lookup var ns
  EData _ dt                          -> return dt
  EApply pos funE valE                -> do
    fun <- exec ns (pos:st) funE
    val <- exec ns (pos:st) valE
    next <- nextId
    addNext (DLazyApply (pos:st) fun val)
    return $ DRef next
  ELetRec pos nameDefs ex             -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _, _), place) m -> M.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, _, e) -> addNext $ DLazyEval new_ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELet pos nameDefs ex                -> do
    next <- nextId
    let new_ns = foldr
          (\((name, _, _), place) m -> M.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, _, e) -> addNext $ DLazyEval ns st e) nameDefs
    exec new_ns (pos:st) ex
  ELambda _ var def                   -> do
    return $ DLambda ns var def
  _                                   -> undefined


evalProgram prog = fst $ runState
  (runExceptT $
    exec M.empty [(Position "<main>" 0 0)] prog >>= derefData [(Position "<main>" 0 0)]
    )
  Sequence.empty
