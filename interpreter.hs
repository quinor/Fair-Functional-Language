import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map


type Var = String

data DataType =
      DLazy Namespace Exp
    | DLambda Namespace Var Exp
    | DPrimitive Primitive
    | DInt Int
    | DBool Bool

data Exp =
    ELambda Var Exp      -- (\Var -> Exp)
  | ELet Var Exp Exp     -- let Var = Exp in Exp, Var visible only in second exp
  | ELetRec Var Exp Exp  -- letrec Var = Exp in Exp, Var visible in both exps
  | EApply Exp Exp       -- (Var Var)
  | EData DataType       -- just DataType constant
  | EVar Var             -- just Var

data Primitive =
    Prim1 (Exp -> Interpreter DataType)
  | Prim2 (Exp -> Exp -> Interpreter DataType)
  | Prim3 (Exp -> Exp -> Exp -> Interpreter DataType)


type Memory = Sequence.Seq DataType
type Namespace = Map.Map Var Int

type Interpreter a = ReaderT Namespace (StateT Memory Identity) a


apply_primitive :: Primitive -> Exp -> Interpreter DataType
apply_primitive (Prim1 fn) ex = fn ex
apply_primitive (Prim2 fn) ex = return $ DPrimitive $ Prim1 $ fn ex
apply_primitive (Prim3 fn) ex = return $ DPrimitive $ Prim2 $ fn ex


wrap_2i_i :: (Int -> Int -> Int) -> Primitive
wrap_2i_i f = Prim2 $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DInt $ f e1 e2

wrap_2i_b :: (Int -> Int -> Bool) -> Primitive
wrap_2i_b f = Prim2 $ \e1 e2 -> do
  DInt e1 <- exec e1
  DInt e2 <- exec e2
  return $ DBool $ f e1 e2

wrap_2b_b :: (Bool -> Bool -> Bool) -> Primitive
wrap_2b_b f = Prim2 $ \e1 e2 -> do
  DBool e1 <- exec e1
  DBool e2 <- exec e2
  return $ DBool $ f e1 e2

p_if = Prim3 $ \e1 e2 e3 -> do
  DBool cond <- exec e1
  if cond
    then exec e2
    else exec e3


p_add = wrap_2i_i (+)
p_sub = wrap_2i_i (-)
p_mul = wrap_2i_i (*)
p_div = wrap_2i_i div
p_sum = wrap_2i_i mod

p_eq = wrap_2i_b (==)
p_neq = wrap_2i_b (/=)
p_lt = wrap_2i_b (<)
p_gt = wrap_2i_b (>)
p_le = wrap_2i_b (<=)
p_ge = wrap_2i_b (>=)

p_and = Prim2 $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then exec e2
  else return $ DBool False

p_or = Prim2 $ \e1 e2 -> do
  DBool e1 <- exec e1
  if e1
    then return $ DBool True
    else exec e2

p_neg = Prim1 $ \e1 -> do
  DBool e1 <- exec e1
  return $ DBool $ not e1


get_value :: Var -> Interpreter DataType
get_value var = do
  id <- reader (\env -> fromJust $ Map.lookup var env)
  dt <- state (\st -> ((fromJust $ Sequence.lookup id st), st))
  case dt of
    DLazy ns exp  -> do
      dt_val <- local (const ns) (exec exp)
      state (\st -> ((), Sequence.update id dt_val st))
      return dt_val
    _             -> return dt

next_id :: Interpreter Int
next_id = state (\st -> (length st, st))

add_next :: DataType -> Interpreter ()
add_next dt = state (\st -> ((), st Sequence.|> dt))

exec :: Exp -> Interpreter DataType
exec ex = case ex of
  EVar var                          -> get_value var
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
  ELetRec name def exp              -> do
    next <- next_id
    ns <- ask
    let new_ns = (Map.insert name next ns)
    add_next (DLazy new_ns def)
    local (const new_ns) (exec exp)
  ELet name def exp                 -> do
    next <- next_id
    ns <- ask
    add_next (DLazy ns def)
    local (Map.insert name next) (exec exp)
  ELambda var def                   -> do
    ns <- ask
    return $ DLambda ns var def

run_program :: Exp -> DataType
run_program prog = fst $ runIdentity $ runStateT (runReaderT (exec prog) Map.empty) Sequence.empty





run_test = if sum_test == 3 && let_test == 42 && lambda_test == 13 && if_test == 42 && fact_test == 120
  then True
  else False


DInt sum_test = run_program (EApply (EApply (EData $ DPrimitive $ p_add) (EData $ DInt 1)) (EData $ DInt 2))

DInt let_test = run_program $ ELet "num"
 (EData $ DInt 42)
 (EVar "num")

DInt lambda_test = run_program $ ELet "add_one"
  (ELambda "n"
    (EApply (EApply (EData $ DPrimitive $ p_add) (EData $ DInt 1)) (EVar "n"))
  )
  (EApply (EVar "add_one") (EData $ DInt 12))

DInt if_test = run_program $ (ELet "if")
  (ELambda "b"
    (EApply (EApply (EApply (EData $ DPrimitive $ p_if)
      (EVar "b"))
      (EData $ DInt 42))
      (EData $ DInt 13))
  )
  (EApply (EVar "if") (EData $ DBool True))

DInt fact_test = run_program $ ELetRec "factorial"
  (ELambda "n"
    (EApply (EApply (EApply (EData $ DPrimitive $ p_if)
      (EApply (EApply (EData $ DPrimitive $ p_eq) (EData $ DInt 0)) (EVar "n")))
      (EData $ DInt 1))
      (EApply (EApply (EData $ DPrimitive $ p_mul)
        (EVar "n"))
        (EApply
          (EVar "factorial")
          ((EApply (EApply (EData $ DPrimitive $ p_sub) (EVar "n")) (EData $ DInt 1)))
        )
      )
    )
  )
  (EApply (EVar "factorial") (EData $ DInt 5))
