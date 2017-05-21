{-# Options -Wall #-}

module Interpreter.Typecheck (
  checkType
) where
import Interpreter.Defs
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe


import Debug.Trace

-- perform type checking on the expression and return necessary substitution
-- and type infered (after the substitution has been made)
inferType :: TypeEnv -> Exp -> TypeChecker (Sub, Type)
-- check returned type of a program
checkType :: Exp -> Either (Position, String) Type


-- type scheme (forall variables type)
data TypeScheme = Scheme [VarT] Type deriving Show


-- substitution of type variables
type Sub = M.Map VarT Type



class Types a where
  freeVars :: a -> S.Set VarT
  applySub :: Sub -> a -> a

newtype TypeEnv = TypeEnv (M.Map VarE TypeScheme)

type TypeChecker = ExceptT (Position, String) (State Int)

--empty substitution
nullSub :: Sub

--compose substitutions
compose :: Sub -> Sub -> Sub

--unification (mgu)
unify :: Position -> Type -> Type -> TypeChecker Sub

--create fresh type variable
newVar :: String -> TypeChecker Type -- TVar vq

--generalize (pull as many free vars as you can)
generalize :: TypeEnv -> Type -> TypeScheme

--substitute all forall vars with fresh ones
instantiate :: TypeScheme -> TypeChecker Type

--
debugMessage :: String -> TypeChecker ()
debugMessage = if False then traceM else \_ -> return ()

--inferType preliminaries

instance Types Type where
  freeVars t = case t of
    TInt          -> S.empty
    TBool         -> S.empty
    TLambda t1 t2 -> freeVars t1 `S.union` freeVars t2
    TVar v        -> S.singleton v
  applySub sub t = case t of
    TInt          -> TInt
    TBool         -> TBool
    TLambda t1 t2 -> TLambda (applySub sub t1) (applySub sub t2)
    TVar v        -> fromMaybe (TVar v) $ M.lookup v sub


instance Types TypeScheme where
  freeVars (Scheme vars t) = freeVars t S.\\ S.fromList vars
  applySub sub (Scheme vars t) = Scheme vars (applySub (foldr M.delete sub vars) t)


instance Types a => Types [a] where
  freeVars = foldr (S.union . freeVars) S.empty
  applySub sub = map (applySub sub)


instance Types TypeEnv where
  freeVars (TypeEnv env) = freeVars $ M.elems env
  applySub sub (TypeEnv env) = TypeEnv $ M.map (applySub sub) env



nullSub = M.empty

compose s1 s2 = M.map (applySub s1) s2 `M.union` s1

unify p t1 t2 = case (t1, t2) of
  (TBool, TBool)              -> return nullSub
  (TInt, TInt)                -> return nullSub
  (TLambda a b, TLambda c d)  -> do
    u1 <- unify p a c
    u2 <- unify p (applySub u1 b) (applySub u1 d)
    return $ u2 `compose` u1
  (TVar a, t)                 -> case t of -- should wipe a out of existence
    TVar x | a == x   -> return nullSub
    x                 -> if a `S.member` freeVars x
      then throwError (p, "occur check") -- occur check
      else return $ M.singleton a x
  -- must be later becouse overlaps with previous one
  (t, TVar a)                 -> unify p (TVar a) t
  (_, _)                      -> throwError
    (p, "expressions don't unify! " ++ show t1 ++ " and " ++ show t2) -- unification error


newVar pref = do
  x <- state (\no -> (no, no+1))
  return $ TVar $ pref ++ show x


generalize env t = let
  vars = S.toList $ freeVars t S.\\ freeVars env
  in Scheme vars t


instantiate (Scheme vars t) = do
  newVars <- mapM (\_ -> newVar "va_inst_") vars
  let s = M.fromList (zip vars newVars)
  return $ applySub s t


--inferType implementation

inferType env@(TypeEnv envDict) expr = case expr of
  EVar pos var                          -> case M.lookup var envDict of
    Nothing -> throwError (pos, "no such variable: " ++ var)
    Just ts -> do
      t <- instantiate ts
      return (nullSub, t)
  EData _ dt                          -> case dt of
    DPrimitive pr -> case pr of
      Prim0 _ t _ -> return (nullSub, t)
      Prim1 _ t _ -> return (nullSub, t)
      Prim2 _ t _ -> return (nullSub, t)
      Prim3 _ t _ -> return (nullSub, t)
    DInt _        -> return (nullSub, TInt)
    DBool _       -> return (nullSub, TBool)
    _             -> undefined -- no literals of other types possible
  EApply pos fun val                    -> do
    tv <- newVar "va_app_"
    (s1, t1) <- inferType env fun
    let env' = applySub s1 env
    (s2, t2) <- inferType env' val
    s3 <- unify pos (applySub s2 t1) (TLambda t2 tv)
    return (s3 `compose` s2 `compose` s1, applySub s3 tv)
  ELetRec pos nameDefs ex              -> do
    --insert type variables
    new <- foldM
      (\ (TypeEnv oEDict) (n, _) -> do
        tv <- newVar "va_letrec_"
        return $ TypeEnv $ M.insert n (Scheme [] tv) oEDict
      )
      env
      nameDefs
    --infer type of clauses
    (sub, new') <- foldM
      (\ (oldSub, oldEnv) (n, e) -> do
        (s, t) <- inferType oldEnv e
        let newEnv@(TypeEnv dict) = applySub s oldEnv
        tv <- (instantiate $ fromJust $ M.lookup n dict)
        s' <- unify pos t tv
        return (s' `compose` s `compose` oldSub, applySub s' newEnv)
      )
      (nullSub, new)
      nameDefs
    --generalize types
    let env' = applySub sub env
    new'' <- foldM
      (\ (TypeEnv dict) (n, _) -> do
        tv <- (instantiate $ fromJust $ M.lookup n dict)
        debugMessage $ "type of " ++ n ++ ": " ++ show (generalize env' tv)
        return $ TypeEnv $ M.insert n (generalize env' tv) dict
      )
      new'
      nameDefs
    (sx, tx) <- inferType new'' ex
    return (sx `compose` sub, tx)
  ELet _ nameDefs ex                 -> do
    (sub, _, new) <- foldM
      (\ (oldSub, oldLightEnv, TypeEnv oREDict) (n, e) -> do
        (s, t) <- inferType oldLightEnv e
        let lightEnv = applySub s oldLightEnv
        debugMessage $ "type of " ++ n ++ ": " ++ show (generalize lightEnv t)
        let richEnv = TypeEnv $ M.insert n (generalize lightEnv t) oREDict
        return (s `compose` oldSub, lightEnv, richEnv)
      )
      (nullSub, env, env)
      nameDefs
    (sx, tx) <- inferType new ex
    return (sx `compose` sub, tx)
  ELambda _ var def                   -> do
    tv <- newVar "va_lambda_"
    let newEnv = TypeEnv $ M.insert var (Scheme [] tv) envDict
    (s, t) <- inferType newEnv def
    return (s, TLambda (applySub s tv) t)

checkType e = (return . snd) =<< (fst $ runState (runExceptT $ inferType (TypeEnv M.empty) e) 0)
