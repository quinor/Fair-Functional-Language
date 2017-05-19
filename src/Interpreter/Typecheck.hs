module Interpreter.Typecheck (
  checkType
) where
import Interpreter.Defs
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe


-- perform type checking on the expression and return necessary substitution
-- and type infered (after the substitution has been made)
inferType :: TypeEnv -> Exp -> TypeChecker (Sub, Type)
-- check returned type of a program
checkType :: Exp -> Either (Position, String) Type


-- type scheme (forall variables type)
data TypeScheme = Scheme [VarT] Type


-- substitution of type variables
type Sub = Map.Map VarT Type



class Types a where
  freeVars :: a -> Set.Set VarT
  applySub :: Sub -> a -> a

newtype TypeEnv = TypeEnv (Map.Map VarE TypeScheme)

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


--inferType preliminaries

instance Types Type where
  freeVars t = case t of
    TInt          -> Set.empty
    TBool         -> Set.empty
    TLambda t1 t2 -> freeVars t1 `Set.union` freeVars t2
    TVar v        -> Set.singleton v
  applySub sub t = case t of
    TInt          -> TInt
    TBool         -> TBool
    TLambda t1 t2 -> TLambda (applySub sub t1) (applySub sub t2)
    TVar v        -> fromMaybe (TVar v) $ Map.lookup v sub


instance Types TypeScheme where
  freeVars (Scheme vars t) = freeVars t Set.\\ Set.fromList vars
  applySub sub (Scheme vars t) = Scheme vars (applySub (foldr Map.delete sub vars) t)


instance Types a => Types [a] where
  freeVars = foldr (Set.union . freeVars) Set.empty
  applySub sub = map (applySub sub)


instance Types TypeEnv where
  freeVars (TypeEnv env) = freeVars $ Map.elems env
  applySub sub (TypeEnv env) = TypeEnv $ Map.map (applySub sub) env



nullSub = Map.empty

compose s1 s2 = s1 `Map.union` Map.map (applySub s1) s2

unify p t1 t2 = case (t1, t2) of
  (TBool, TBool)                  -> return nullSub
  (TInt, TInt)                    -> return nullSub
  (TLambda a b, TLambda c d)  -> do
    u1 <- unify p a c
    u2 <- unify p (applySub u1 b) (applySub u1 d)
    return $ u1 `compose` u2
  (t, TVar a)                   -> unify p (TVar a) t
  (TVar a, t)                   -> case t of
    TVar x | a == x   -> return nullSub
    x                 -> if a `Set.member` freeVars x
      then throwError (p, "occur check") -- occur check
      else return $ Map.singleton a x
  (_, _)                          -> throwError (p, "expressions don't unify!") -- error


newVar pref = do
  x <- state (\no -> (no, no+1))
  return $ TVar $ pref ++ show x


generalize env t = let
  vars = Set.toList $ freeVars t Set.\\ freeVars env
  in Scheme vars t


instantiate (Scheme vars t) = do
  newVars <- mapM (\_ -> newVar "va_inst_") vars
  let s = Map.fromList (zip vars newVars)
  return $ applySub s t

--inferType implementation

inferType env@(TypeEnv envDict) ex = case ex of
  EVar pos var                          -> case Map.lookup var envDict of
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
    return (s1 `compose` s2 `compose` s3, applySub s3 tv)
  ELetRec pos nameDefs ex              -> do -- TODO: multiple defs
    --insert type variables
    new <- foldM
      (\ oldEnv@(TypeEnv oEDict) (n, _) -> do
        tv <- newVar "va_letrec_"
        return $ TypeEnv $ Map.insert n (Scheme [] tv) oEDict
      )
      env
      nameDefs
    --infer type of clauses
    (sub, new') <- foldM
      (\ (oldSub, oldEnv) (n, e) -> do
        (s, t) <- inferType oldEnv e
        let oldEnv'@(TypeEnv dict) = applySub s oldEnv
        tv <- (instantiate $ fromJust $ Map.lookup n dict)
        s' <- unify pos t tv
        return (oldSub `compose` s `compose` s', applySub s' oldEnv')
      )
      (nullSub, new)
      nameDefs
    (sx, tx) <- inferType new' ex
    return (sub `compose` sx, tx)
  ELet _ nameDefs ex                 -> do
    (sub, _, new) <- foldM
      (\ (oldSub, oldLightEnv, oldRichEnv@(TypeEnv oREDict)) (n, e) -> do
        (s, t) <- inferType oldLightEnv e
        let lightEnv = applySub s oldLightEnv
        let richEnv = TypeEnv $ Map.insert n (generalize lightEnv t) oREDict
        return (oldSub `compose` s, lightEnv, richEnv)
      )
      (nullSub, env, env)
      nameDefs
    (sx, tx) <- inferType new ex
    return (sub `compose` sx, tx)
  ELambda _ var def                   -> do
    tv <- newVar "va_lambda_"
    let newEnv = TypeEnv $ Map.insert var (Scheme [] tv) envDict
    (s, t) <- inferType newEnv def
    return (s, TLambda (applySub s tv) t)

checkType e = (return . snd) =<< (fst $ runState (runExceptT $ inferType (TypeEnv Map.empty) e) 0)
--(lift snd) $ fst $
