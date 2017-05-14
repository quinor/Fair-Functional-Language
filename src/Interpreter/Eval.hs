{-# Language FlexibleContexts #-}

module Interpreter.Eval (
  eval_program,
  exec
) where
import Interpreter.Defs
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map

-- execute the expression (AST) and return the result
exec :: Exp -> Interpreter Data
-- run expression and retrieve the result
eval_program :: Exp -> Data

-- primitive applivation function
apply_primitive :: Primitive -> Exp -> Interpreter Data



apply_primitive (Prim1 _ fn) ex = fn ex
apply_primitive (Prim2 (TLambda _ b) fn) ex = return $ DPrimitive $ Prim1 b $ fn ex
apply_primitive (Prim3 (TLambda _ b) fn) ex = return $ DPrimitive $ Prim2 b $ fn ex


-- exec helper functions for state monad

next_id :: MonadState (Sequence.Seq a) m => m Int
next_id = state (\st -> (length st, st))

add_next :: MonadState (Sequence.Seq a) m => a -> m ()
add_next dt = state (\st -> ((), st Sequence.|> dt))


-- exec implementation

exec ex = case ex of
  EVar var                          -> do
    id <- reader (\env -> fromJust $ Map.lookup var env)
    dt <- state (\st -> ((fromJust $ Sequence.lookup id st), st))
    case dt of
      DLazy ns ex   -> do
        dt_val <- local (const ns) (exec ex)
        state (\st -> ((), Sequence.update id dt_val st))
        return dt_val
      _             -> return dt
  EData dt                          -> return dt
  EApply fun val                    -> do
    fun <- exec fun
    case fun of
      DLambda f_ns var def  -> do
        next <- next_id
        ns <- ask
        add_next (DLazy ns val)
        local (const $ Map.insert var next f_ns) (exec def)
      DPrimitive prim       -> apply_primitive prim val
      _                     -> undefined -- you can apply nothig else
  ELetRec name def ex               -> do
    next <- next_id
    ns <- ask
    let new_ns = (Map.insert name next ns)
    add_next (DLazy new_ns def)
    local (const new_ns) (exec ex)
  ELet name def ex                  -> do
    next <- next_id
    ns <- ask
    add_next (DLazy ns def)
    local (Map.insert name next) (exec ex)
  ELambda var def                   -> do
    ns <- ask
    return $ DLambda ns var def

eval_program prog = fst $ runIdentity $ runStateT (runReaderT (exec prog) Map.empty) Sequence.empty
