module Interpreter.Defs (
  VarE,
  VarT,
  Data(..),
  Position(..),
  Exp(..),
  TLD(..),
  Type(..),
  Primitive(..),
  Interpreter,
  takePos
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

-- position in the source file: filename, line, column
data Position = Position String Int Int deriving Show

-- basically AST
data Exp =
    ELambda Position VarE Exp          -- (\Var -> Exp)
  | ELet    Position [(VarE, Exp)] Exp    -- let [Var = Exp] in Exp, Vars visible only in second exp
  | ELetRec Position [(VarE, Exp)] Exp -- letrec [Var = Exp] in Exp, Vars visible in both exps
  | EApply  Position Exp Exp            -- (Var Var)
  | EData   Position Data                -- just Data constant
  | EVar    Position VarE                 -- just Var
  deriving Show

takePos :: Exp -> Position
takePos (ELambda p _ _) = p
takePos (ELet    p _ _) = p
takePos (ELetRec p _ _) = p
takePos (EApply  p _ _) = p
takePos (EData   p _  ) = p
takePos (EVar    p _  ) = p


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
