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
  takePos,
  takeName
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


-- primitive function with type, string is primitive name (for docs/error purposes)
data Primitive =
    Prim1 String Type (Exp -> Interpreter Data)
  | Prim2 String Type (Exp -> Exp -> Interpreter Data)
  | Prim3 String Type (Exp -> Exp -> Exp -> Interpreter Data)

takeName :: Primitive -> String
takeName (Prim1 n _ _) = n
takeName (Prim2 n _ _) = n
takeName (Prim3 n _ _) = n


-- ====================== CONVENIENCE SHOW CLASSES ============================

ppShow :: Show a => a -> PP.Doc
ppShow = PP.text . show


instance Show Data where
  showsPrec _ x = shows (prData x)

prData :: Data -> PP.Doc
prData (DInt x) = ppShow x
prData (DBool b) = ppShow b
prData (DPrimitive p) = ppShow p
prData (DLambda _ _ _) = PP.text "Lambda function"



instance Show Exp where
  showsPrec _ x = shows $ prExp x

letBlock :: [(VarE, Exp)] -> PP.Doc
letBlock a = foldl1
  (\t1 t2 -> (t1 PP.<+> PP.text "and") PP.$$ t2)
  (map ((PP.nest 2) . \(n, e) -> PP.text n PP.<+> PP.text "=" PP.<+> prExp e) a)


prExp :: Exp -> PP.Doc
prExp (EVar _ n) = PP.text n
prExp (EData _ d) = prData d
prExp (EApply _ e1 e2) = prExp e1 PP.<+> prParenExp e2
prExp (ELet _ l e) =
          PP.text "let" PP.$$ letBlock l
  PP.$$  (PP.nest 0) (PP.text "in" PP.<+> prExp e)
prExp (ELetRec _ l e) =
          PP.text "rec" PP.$$ letBlock l
  PP.$$  (PP.nest 0) (PP.text "in" PP.<+> prExp e)
prExp (ELambda _ n e) = PP.text "\\" PP.<+> PP.text n PP.<+> PP.text "->" PP.<+> prExp e

prParenExp :: Exp -> PP.Doc
prParenExp t = case t of
  ELet _ _ _    -> PP.parens $ prExp t
  ELetRec _ _ _ -> PP.parens $ prExp t
  ELambda _ _ _ -> PP.parens $ prExp t
  EApply _ _ _  -> PP.parens $ prExp t
  _             -> prExp t



instance Show Type where
  showsPrec _ x = shows $ prType x

prType :: Type -> PP.Doc
prType TInt           = PP.text "Int"
prType TBool          = PP.text "Bool"
prType (TLambda a b)  = prParenType a PP.<+> PP.text "->" PP.<+> prType b
prType (TVar n)       = PP.text n

prParenType :: Type -> PP.Doc
prParenType t = case t of
  TLambda _ _ -> PP.parens $ prType t
  _           -> prType t



instance Show Primitive where
  showsPrec _ x = shows $ prPrim x

prPrim :: Primitive -> PP.Doc
prPrim (Prim1 n t _) = PP.text "Prim1" PP.<+> PP.text n PP.<+> PP.parens (prType t)
prPrim (Prim2 n t _) = PP.text "Prim2" PP.<+> PP.text n PP.<+> PP.parens (prType t)
prPrim (Prim3 n t _) = PP.text "Prim3" PP.<+> PP.text n PP.<+> PP.parens (prType t)
