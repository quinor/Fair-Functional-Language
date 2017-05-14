module Interpreter.Typecheck (
  check_type
) where
import Interpreter.Defs
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

-- perform type checking on the expression and return necessary substitution
-- and type infered (after the substitution has been made)
infer_type :: TypeEnv -> Exp -> TypeChecker (Sub, Type)
-- check returned type of a program
check_type :: Exp -> Type


-- type scheme (forall variables type)
data TypeScheme = Scheme [VarT] Type


-- substitution of type variables
type Sub = Map.Map VarT Type



class Types a where
  free_vars :: a -> Set.Set VarT
  apply_sub :: Sub -> a -> a

newtype TypeEnv = TypeEnv (Map.Map VarE TypeScheme)

type TypeChecker a = StateT Int Identity a

--empty substitution
null_sub :: Sub

--error
throw :: String -> TypeChecker a

--compose substitutions
compose :: Sub -> Sub -> Sub

--unification (mgu)
unify :: Type -> Type -> TypeChecker Sub

--create fresh type variable
new_var :: String -> TypeChecker Type -- TVar vq

--generalize (pull as many free vars as you can)
generalize :: TypeEnv -> Type -> TypeScheme

--substitute all forall vars with fresh ones
instantiate :: TypeScheme -> TypeChecker Type


--infer_type preliminaries

instance Types Type where
  free_vars t = case t of
    TInt          -> Set.empty
    TBool         -> Set.empty
    TLambda t1 t2 -> (free_vars t1) `Set.union` (free_vars t2)
    TVar v        -> Set.singleton v
  apply_sub sub t = case t of
    TInt          -> TInt
    TBool         -> TBool
    TLambda t1 t2 -> TLambda (apply_sub sub t1) (apply_sub sub t2)
    TVar v        -> case Map.lookup v sub of
      Just x  -> x
      Nothing -> TVar v


instance Types TypeScheme where
  free_vars (Scheme vars t) = (free_vars t) Set.\\ (Set.fromList vars)
  apply_sub sub (Scheme vars t) = Scheme vars (apply_sub (foldr Map.delete sub vars) t)


instance Types a => Types [a] where
  free_vars l = foldr Set.union Set.empty (map free_vars l)
  apply_sub sub = map (apply_sub sub)


instance Types TypeEnv where
  free_vars (TypeEnv env) = free_vars $ Map.elems env
  apply_sub sub (TypeEnv env) = TypeEnv $ Map.map (apply_sub sub) env



null_sub = Map.empty

throw s = trace s undefined --todo: fuckin' fix it!

compose s1 s2 = s1 `Map.union` (Map.map (apply_sub s1) s2)

unify t1 t2 = case (t1, t2) of
  (TBool, TBool)                  -> return null_sub
  (TInt, TInt)                    -> return null_sub
  ((TLambda a b), (TLambda c d))  -> do
    u1 <- unify a c
    u2 <- unify (apply_sub u1 b) (apply_sub u1 d)
    return $ u1 `compose` u2
  (t, (TVar a))                   -> unify (TVar a) t
  ((TVar a), t)                   -> case t of
    TVar x | a == x   -> return null_sub
    x                 -> if a `Set.member` (free_vars x)
      then throw "occur check" -- occur check
      else return $ Map.singleton a x
  (_, _)                          -> throw "expressions don't unify!" -- error


new_var pref = do
  x <- state (\no -> (no, no+1))
  return $ TVar $ pref ++ (show x)


generalize env t = let
  vars = Set.toList $ (free_vars t) Set.\\ (free_vars env)
  in Scheme vars t


instantiate (Scheme vars t) = do
  new_vars <- mapM (\_ -> new_var "va_in_") vars
  let s = Map.fromList (zip vars new_vars)
  return $ apply_sub s t

--infer_type implementation

infer_type env@(TypeEnv env_dict) ex = case ex of
  EVar var                          -> case Map.lookup var env_dict of
    Nothing -> throw $ "no such variable: " ++ var
    Just ts -> do
      t <- instantiate ts
      return (null_sub, t)
  EData dt                          -> case dt of
    DPrimitive pr -> case pr of
      Prim1 t _ -> return (null_sub, t)
      Prim2 t _ -> return (null_sub, t)
      Prim3 t _ -> return (null_sub, t)
    DInt _        -> return (null_sub, TInt)
    DBool _       -> return (null_sub, TBool)
    _             -> throw "how did you manage to get suc data literal?" -- no literals of other types allowed
  EApply fun val                    -> do
    tv <- new_var "va_app_"
    (s1, t1) <- infer_type env fun
    let env' = apply_sub s1 env
    (s2, t2) <- infer_type env' val
    s3 <- unify (apply_sub s2 t1) (TLambda t2 tv)
    return $ (s1 `compose` s2 `compose` s3, (apply_sub s3 tv))
  ELetRec name def ex               -> do --TODO: fix it!
    tv <- new_var "va_letrec_"
    (s1, t1) <- infer_type (TypeEnv $ Map.insert name (Scheme [] tv) env_dict) def
    s2 <- unify t1 (apply_sub s1 tv)
    let
      env'@(TypeEnv env'_dict) = (apply_sub (s1 `compose` s2) env)
      new_env = TypeEnv $ Map.insert name (generalize env' (apply_sub s2 t1)) env'_dict
    (s3, t3) <- infer_type new_env ex
    return $ (s1 `compose` s2 `compose` s3, t3)
  ELet name def ex                  -> do
    (s1, t1) <- infer_type env def
    let
      env'@(TypeEnv env'_dict) = apply_sub s1 env
      new_env = TypeEnv $ Map.insert name (generalize env' t1) env'_dict
    (s2, t2) <- infer_type new_env ex
    return $ (s1 `compose` s2, t2)
  ELambda var def                   -> do
    tv <- new_var "va_lambda_"
    let new_env = TypeEnv $ Map.insert var (Scheme [] tv) env_dict
    (s, t) <- infer_type new_env def
    return (s, TLambda (apply_sub s tv) t)

check_type e = snd $ fst $ runIdentity $ runStateT (infer_type (TypeEnv Map.empty) e) 0

