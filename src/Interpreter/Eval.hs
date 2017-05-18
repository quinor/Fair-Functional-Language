{-# Language FlexibleContexts #-}

module Interpreter.Eval (
  evalProgram,
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
evalProgram :: Exp -> Data

-- primitive applivation function
applyPrimitive :: Primitive -> Exp -> Interpreter Data



applyPrimitive (Prim1 _ fn) ex = fn ex
applyPrimitive (Prim2 (TLambda _ b) fn) ex = return $ DPrimitive $ Prim1 b $ fn ex
applyPrimitive (Prim3 (TLambda _ b) fn) ex = return $ DPrimitive $ Prim2 b $ fn ex


-- exec helper functions for state monad

nextId :: MonadState (Sequence.Seq a) m => m Int
nextId = state (\st -> (length st, st))

addNext :: MonadState (Sequence.Seq a) m => a -> m ()
addNext dt = state (\st -> ((), st Sequence.|> dt))


-- exec implementation

exec ex = case ex of
  EVar var                          -> do
    id <- reader (fromJust . Map.lookup var)
    dt <- state (\st -> (st `Sequence.index` id, st))
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
        next <- nextId
        ns <- ask
        addNext (DLazy ns val)
        local (const $ Map.insert var next f_ns) (exec def)
      DPrimitive prim       -> applyPrimitive prim val
      _                     -> undefined -- you can apply nothig else
  ELetRec nameDefs ex              -> do
    next <- nextId
    ns <- ask
    let new_ns = foldr
          (\((name, _), place) m -> Map.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy new_ns e) nameDefs
    local (const new_ns) (exec ex)
  ELet nameDefs ex                 -> do
    next <- nextId
    ns <- ask
    let new_ns = foldr
          (\((name, _), place) m -> Map.insert name place m)
          ns
          (zip nameDefs [next..])
    mapM_ (\(_, e) -> addNext $ DLazy ns e) nameDefs
    local (const new_ns) (exec ex)
  ELambda var def                   -> do
    ns <- ask
    return $ DLambda ns var def

evalProgram prog = fst $ runIdentity $ runStateT (runReaderT (exec prog) Map.empty) Sequence.empty
