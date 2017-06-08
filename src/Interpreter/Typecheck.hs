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

--apply type annotation and return type scheme
resolveAnnotation :: TypeEnv -> Position -> Maybe Type -> Type -> TypeChecker TypeScheme

--debug. ugly.
debugMessage :: String -> TypeChecker ()
debugMessage = if True then traceM else \_ -> return ()

--inferType preliminaries

instance Types Type where
  freeVars t = case t of
    TInt            -> S.empty
    TBool           -> S.empty
    TLambda t1 t2   -> freeVars t1 `S.union` freeVars t2
    TAlgebraic _ l  -> S.unions $ map freeVars l
    TVar v          -> S.singleton v
    TUserVar _      -> S.empty
  applySub sub t = case t of
    TInt            -> TInt
    TBool           -> TBool
    TLambda t1 t2   -> TLambda (applySub sub t1) (applySub sub t2)
    TAlgebraic n l  -> TAlgebraic n $ map (applySub sub) l
    TVar v          -> fromMaybe (TVar v) $ M.lookup v sub
    TUserVar _      -> t


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
  (TBool, TBool)                        -> return nullSub
  (TInt, TInt)                          -> return nullSub
  (TLambda a b, TLambda c d)            -> do
    u1 <- unify p a c
    u2 <- unify p (applySub u1 b) (applySub u1 d)
    return $ u2 `compose` u1
  (TAlgebraic n1 l1, TAlgebraic n2 l2)  -> if n1 == n2 && length l1 == length l2
    then do
      resultSub <- foldM
        (\oldSub (te1, te2) -> do
          sub <- unify p (applySub oldSub te1) (applySub oldSub te2)
          return $ sub `compose` oldSub
        )
        nullSub
        (zip l1 l2)
      return resultSub
    else throwError (p, "expressions don't unify! " ++ show t1 ++ " and " ++ show t2)
  (TVar a, t)                           -> case t of -- should wipe a out of existence
    TVar x | a == x   -> return nullSub
    x                 -> if a `S.member` freeVars x
      then throwError (p, "occur check") -- occur check
      else return $ M.singleton a x
  -- must be later becouse overlaps with previous one
  (t, TVar a)                           -> unify p (TVar a) t
  (TUserVar a, TUserVar b)
    | a == b                            -> return nullSub
  (_, _)                                -> throwError
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


resolveAnnotation env pos ann t = case ann of
  Just a -> do
    void $ unify pos t a
    return $ generalize (TypeEnv M.empty) (removeUser a)
  Nothing -> return $ generalize env t

-- inferType implementation
inferType env@(TypeEnv envDict) expr = case expr of
  EVar pos var                          -> case M.lookup var envDict of
    Nothing -> throwError (pos, "no such variable: " ++ var)
    Just ts -> do
      t <- instantiate ts
      return (nullSub, t)
  EData _ dt                          -> case dt of
    DPrimitive (Prim _ t _ _) -> return (nullSub, t)
    DInt _                    -> return (nullSub, TInt)
    DBool _                   -> return (nullSub, TBool)
    _                         -> undefined -- no literals of other types possible
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
      (\ (TypeEnv oEDict) (n, _, _) -> do
        tv <- newVar "va_letrec_"
        return $ TypeEnv $ M.insert n (Scheme [] tv) oEDict
      )
      env
      nameDefs
    --infer type of clauses
    (sub, new') <- foldM
      (\ (oldSub, oldEnv) (n, _, e) -> do
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
      (\ (TypeEnv dict) (n, ann, _) -> do
        tv <- (instantiate $ fromJust $ M.lookup n dict)
        gen_t <- resolveAnnotation env' pos ann tv
        debugMessage $ "type of " ++ n ++ ": " ++ show gen_t
        return $ TypeEnv $ M.insert n gen_t dict
      )
      new'
      nameDefs
    (sx, tx) <- inferType new'' ex
    return (sx `compose` sub, tx)
  ELet pos nameDefs ex               -> do
    (sub, _, new) <- foldM
      (\ (oldSub, oldLightEnv, TypeEnv oREDict) (n, ann, e) -> do
        (s, t) <- inferType oldLightEnv e
        let lightEnv = applySub s oldLightEnv
        gen_t <- resolveAnnotation lightEnv pos ann t
        debugMessage $ "type of " ++ n ++ ": " ++ show gen_t
        return (s `compose` oldSub, lightEnv, TypeEnv $ M.insert n gen_t oREDict)
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
  _                                   -> undefined

checkType e = (return . snd) =<< (fst $ runState (runExceptT $ inferType (TypeEnv M.empty) e) 0)
