{-# Options -Wall #-}

module Interpreter.Defs (
  VarE,
  VarT,
  Data(..),
  Position(..),
  Exp(..),
  Ass(..),
  Op(..),
  OpMap,
  TLD(..),
  Type(..),
  Primitive(..),
  Dataspace,
  Namespace,
  Stacktrace,
  Interpreter,
  takePos,
  takeName,
  printStacktrace
) where
import qualified Data.Sequence as Sequence
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except
import qualified Text.PrettyPrint as PP


type VarE = String
type VarT = String


type Dataspace = Sequence.Seq Data
type Namespace = M.Map VarE Int
type Stacktrace = [Position]
type Interpreter a = ExceptT (Stacktrace, String) (State Dataspace) a

-- data type in the language
data Data =
      DRef Int
    | DUndefined
    | DLazy Namespace Stacktrace Exp
    | DLambda Namespace VarE Exp
    | DPrimitive Primitive
    | DInt Int
    | DBool Bool

-- position in the source file: filename, line, column
data Position = Position String Int Int

-- basically AST
data Exp =
    ELambda Position VarE Exp                       -- (\Var -> Exp)
  | ELet    Position [(VarE, Maybe Type, Exp)] Exp  -- let [Var = Exp] in Exp, Vars visible only in second exp
  | ELetRec Position [(VarE, Maybe Type, Exp)] Exp  -- letrec [Var = Exp] in Exp, Vars visible in both exps
  | EApply  Position Exp Exp                        -- (Var Var)
  | EData   Position Data                           -- just Data constant
  | EVar    Position VarE                           -- just Var
  | EOpExpr Position Exp [(String, Exp)]            -- fake type for parsing (before ops are known)

-- position getter for expressions
takePos :: Exp -> Position


-- associativity of operators
data Ass = AssL | AssR deriving (Eq, Show)

-- operator
data Op = Op Int Ass VarE deriving Show -- precedence, associativity, function name

-- operator map
type OpMap = M.Map String Op


-- toplevel definition in a program
data TLD =
    NamedExp VarE (Maybe Type) Exp
  deriving Show

-- type of an expression
data Type =
    TInt
  | TBool
  | TLambda Type Type
  | TVar VarT
  | TUserVar VarT


-- primitive function with type, string is primitive name (for docs/error purposes)
-- stacktrace argument is for failing when primitive fails (currently div by 0 only)
data Primitive =
    Prim0 String Type (Stacktrace -> Interpreter Data)
  | Prim1 String Type (Data -> Stacktrace -> Interpreter Data)
  | Prim2 String Type (Data -> Data -> Stacktrace -> Interpreter Data)
  | Prim3 String Type (Data -> Data -> Data -> Stacktrace -> Interpreter Data)

-- name getter for primitives
takeName :: Primitive -> String

-- "show" for stacktrace
printStacktrace :: Stacktrace -> String



-- ============================= HELPER FUNCTIONS =============================

takePos (ELambda p _ _) = p
takePos (ELet    p _ _) = p
takePos (ELetRec p _ _) = p
takePos (EApply  p _ _) = p
takePos (EData   p _  ) = p
takePos (EVar    p _  ) = p
takePos (EOpExpr p _ _) = p

takeName (Prim0 n _ _) = n
takeName (Prim1 n _ _) = n
takeName (Prim2 n _ _) = n
takeName (Prim3 n _ _) = n

printStacktrace l = foldr1 (++) (map (\e -> show e ++ "\n") l)

-- ========================= CONVENIENCE SHOW CLASSES =========================



ppShow :: Show a => a -> PP.Doc
ppShow = PP.text . show


instance Show Data where
  showsPrec _ x = shows (prData x)

prData :: Data -> PP.Doc
prData (DRef x) = PP.text "ref" PP.<+> ppShow x
prData DUndefined = PP.text "undefined"
prData (DInt x) = ppShow x
prData (DBool b) = ppShow b
prData (DPrimitive p) = ppShow p
prData (DLambda _ _ _) = PP.text "Lambda function"
prData (DLazy _ _ _) = undefined -- non-printable


instance Show Position where
  showsPrec _ (Position fn ln cl) = showString $ fn ++ ":" ++ show ln ++ ":" ++ show cl


instance Show Exp where
  showsPrec _ x = shows $ prExp x

letBlock :: [(VarE, Maybe Type, Exp)] -> PP.Doc
letBlock a = foldr1
  (\t1 t2 -> (t1 PP.<+> PP.text "and") PP.$$ t2)
  (map ((PP.nest 2) . \(n, ann, e) ->
    PP.text n
    PP.<+> case ann of
      Nothing -> PP.empty
      Just an -> PP.text "::" PP.<+> prType an
    PP.<+> PP.text "="
    PP.<+> prExp e) a)


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
prExp (EOpExpr _ e l) = PP.text $ show e ++ " " ++ show (map snd l)

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
prType (TUserVar n)   = PP.text $ "<" ++ n ++ ">"

prParenType :: Type -> PP.Doc
prParenType t = case t of
  TLambda _ _ -> PP.parens $ prType t
  _           -> prType t



instance Show Primitive where
  showsPrec _ x = shows $ prPrim x

prPrim :: Primitive -> PP.Doc
prPrim (Prim0 n t _) = PP.text "Prim0" PP.<+> PP.text n PP.<+> PP.parens (prType t)
prPrim (Prim1 n t _) = PP.text "Prim1" PP.<+> PP.text n PP.<+> PP.parens (prType t)
prPrim (Prim2 n t _) = PP.text "Prim2" PP.<+> PP.text n PP.<+> PP.parens (prType t)
prPrim (Prim3 n t _) = PP.text "Prim3" PP.<+> PP.text n PP.<+> PP.parens (prType t)
