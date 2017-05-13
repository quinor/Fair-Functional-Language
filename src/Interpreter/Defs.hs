module Interpreter.Defs (
  VarE,
  VarT,
  Data(..),
  Exp(..),
  TLD(..),
  Type(..),
  Primitive(..),
  Interpreter
) where
import qualified Data.Sequence as Sequence
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity
import qualified Text.PrettyPrint as PP


type VarE = String
type VarT = String


type Dataspace = Sequence.Seq Data
type Namespace = Map.Map VarE Int
type Interpreter a = ReaderT Namespace (StateT Dataspace Identity) a



-- data type in the language
data Data =
      DLazy Namespace Exp
    | DLambda Namespace VarE Exp
    | DPrimitive Primitive
    | DInt Int
    | DBool Bool
  deriving Show


-- basically AST
data Exp =
    ELambda VarE Exp      -- (\Var -> Exp)
  | ELet VarE Exp Exp     -- let Var = Exp in Exp, Var visible only in second exp
  | ELetRec VarE Exp Exp  -- letrec Var = Exp in Exp, Var visible in both exps
  | EApply Exp Exp       -- (Var Var)
  | EData Data           -- just Data constant
  | EVar VarE             -- just Var
  deriving Show


-- toplevel definition in a program
data TLD =
    NamedExp VarE Exp
  deriving Show

-- type of an expression
data Type =
    TInt
  | TBool
  | TLambda Type Type    -- lambda == primitive as for the type
  | TVar VarT


-- primitive function with type
data Primitive =
    Prim1 Type (Exp -> Interpreter Data)
  | Prim2 Type (Exp -> Exp -> Interpreter Data)
  | Prim3 Type (Exp -> Exp -> Exp -> Interpreter Data)


-- ====================== CONVENIENCE SHOW CLASSES ============================



instance Show Type where
  showsPrec _ x = shows (prType x)

prType :: Type -> PP.Doc
prType TInt           = PP.text "Int"
prType TBool          = PP.text "Bool"
prType (TLambda a b)  = prParenType a PP.<+> PP.text "->" PP.<+> prType b
prType (TVar n)       = PP.text n

prParenType :: Type -> PP.Doc
prParenType t = case t of
  TLambda _ _ -> PP.parens (prType t)
  _           -> prType t



instance Show Primitive where
  showsPrec _ x = shows (prPrim x)


prPrim :: Primitive -> PP.Doc
prPrim (Prim1 t _) = PP.text "Primitive " PP.<+> PP.parens (prType t)
prPrim (Prim2 t _) = PP.text "Primitive " PP.<+> PP.parens (prType t)
prPrim (Prim3 t _) = PP.text "Primitive " PP.<+> PP.parens (prType t)
