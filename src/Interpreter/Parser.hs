{-# Options -Wall #-}

module Interpreter.Parser (
  parseStr,
  ParseResult
) where
import Interpreter.Defs
import Interpreter.Primitives (builtinPrefix)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M
import qualified Data.Graph as G
import qualified Data.Set as S


type ParseResult = Either (ParseError (Token String) Dec) [TLD]

--parse program from string
parseStr :: String -> String -> ParseResult
parseStr = parse langParser

--parser of the entire language
langParser :: Parser [TLD]


-- parser utility functions and definitions
reserved :: [String]
getPos :: Parser Position
sc :: Parser ()
rword :: String -> Parser ()
rop :: String -> Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
parens :: Parser a -> Parser a

lIdentifier :: Parser String
uIdentifier :: Parser String


-- expr parsing
eBoolean :: Parser Exp
eInteger :: Parser Exp
eVariable :: Parser Exp
operator :: Parser String
eExpr :: Parser Exp
eExprSingle :: Parser Exp
eLambda :: Parser Exp
letDecls :: Parser [(VarE, Maybe Type, Exp)]
eLet :: Parser Exp
eRec :: Parser Exp
eIf :: Parser Exp

orderRecClauses :: Position -> [(VarE, Maybe Type, Exp)] -> Exp -> Exp
getVars :: Exp -> S.Set VarE


-- type parsing
tInt :: Parser Type
tBool :: Parser Type
tLambda :: Parser Type
tVar :: Parser Type
tTypeSingle :: Parser Type
tType :: Parser Type
tTypeAnnotation :: Parser (Maybe Type)

--expr postproc
defOpMap :: OpMap
applyOp :: Op -> Exp -> Exp -> Exp
exprConversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
finishOps :: OpMap -> Exp -> Parser Exp

-- toplevel parsing
namedExp :: Parser TLD
toplevelDef :: Parser TLD



-- utility/defs
reserved = ["def", "data", "type", "True", "False", "let", "rec", "and", "match", "with", "in", "if",
  "then", "else", "\\", "->", "<-", "=", "Integer", "Boolean"]

getPos = do
  p <- getPosition
  return $ Position
    (sourceName p)
    (fromIntegral $ unPos $ sourceLine p)
    (fromIntegral $ unPos $ sourceColumn p)

sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

rword s = try $ string s *> notFollowedBy (alphaNumChar <|> char '_') *> sc

rop s = try $ string s *> notFollowedBy (oneOf "=-.:/\\~<>!@$%^&*()+{}|?") *> sc

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

lIdentifier = try $ do
  s <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> char '_')
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s

uIdentifier = try $ do
  s <- lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> char '_')
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ " cannot be an identifier"
    else return s


-- expr
operator = try $ do
  s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?") -- reserved: [](),_"'`
  if s `elem` reserved
    then fail $ "forbidden keyword symbol: " ++ show s ++ " cannot be an operator"
    else return $ s

eExpr = do
  p <- getPos
  h <- eExprSingle
  t <- many (try $ (,) <$> operator <*> eExprSingle)
  return $ EOpExpr p h t

eExprSingle =
      eBoolean
  <|> eInteger
  <|> eVariable
  <|> eLambda
  <|> eLet
  <|> eRec
  <|> eIf
  <|> parens eExpr

eBoolean = do
  p <- getPos
  b <- (rword "True" *> pure True) <|> (rword "False" *> pure False)
  return $ EData p $ DBool b

eInteger = do
  p <- getPos
  i <- fromInteger <$> lexeme L.integer
  return $ EData p $ DInt i

eVariable = do
  p <- getPos
  ident <- lIdentifier
  return $ EVar p ident

eLambda = do
  p <- getPos
  rop "\\"
  vars <- many lIdentifier
  rop "->"
  e <- eExpr
  return $ foldr (ELambda p) e vars

letDecls = (\x -> sepBy1 x (rword "and")) $ do
  v <- lIdentifier
  ann <- tTypeAnnotation
  rop "="
  e <- eExpr
  return (v, ann, e)

eLet = do
  p <- getPos
  rword "let"
  vars <- letDecls
  rword "in"
  e2 <- eExpr
  return $ ELet p vars e2

eRec = do
  p <- getPos
  rword "rec"
  vars <- letDecls
  rword "in"
  e2 <- eExpr
  return $ orderRecClauses p vars e2

eIf = do
  p0 <- getPos
  rword "if"
  p1 <- getPos
  cond <- eExpr
  rword "then"
  p2 <- getPos
  e1 <- eExpr
  rword "else"
  p3 <- getPos
  e2 <- eExpr
  return $ EApply p3 (EApply p2 (EApply p1 (EVar p0 $ builtinPrefix ++ "if") cond) e1) e2


-- get all variables appearing in an expression that were not defined in it,
-- for ordering rec clauses in orderRecClauses
getVars ex = case ex of
  EVar _ v        -> S.singleton v
  EData _ _       -> S.empty
  EApply _ e1 e2  -> getVars e1 `S.union` getVars e2
  ELet _ l e      ->
    (S.unions $ map (getVars . \(_,_,a) -> a)l) `S.union` (getVars e S.\\ (S.fromList $ map (\(a,_,_) -> a) l))
  ELetRec _ l e   ->
    (getVars e `S.union` (S.unions $ map (getVars . (\(_,_,a) -> a)) l)) S.\\ (S.fromList $ map (\(a,_,_) -> a) l)
  ELambda _ n e   -> getVars e S.\\ S.singleton n
  EOpExpr _ e l   -> getVars e `S.union` S.unions (map (\(_,eu) -> getVars eu) l)

orderRecClauses p l ex = let
  labels = map (\(a,_,_) -> a) l
  gr = map (\(n, ann, e) -> ((n, ann, e), n, filter (\a -> S.member a $ getVars e) labels)) l
  scc = map G.flattenSCC $ G.stronglyConnComp gr
  in foldr (ELetRec p) ex scc


-- type
tInt = try $ do
  rword "Int"
  return TInt

tBool = try $ do
  rword "Bool"
  return TBool

tLambda = try $ do -- I regret that try but it has to be there...
  t1 <- tTypeSingle
  rop "->"
  t2 <- tType
  return $ TLambda t1 t2

tVar = try $ do
  a <- lIdentifier
  return $ TUserVar a

tTypeSingle = do
      tInt
  <|> tBool
  <|> tVar
  <|> parens tType

tType = do
      tLambda
  <|> tTypeSingle

tTypeAnnotation = optional $ do
  rop "::"
  tType

-- expr postprocess
defOpMap = M.fromList [
  ("$", Op 0 AssR "__special__lambda"),
  ("||", Op 2 AssL $ builtinPrefix ++ "or"),
  ("&&", Op 3 AssL $ builtinPrefix ++ "and"),
  ("==", Op 4 AssL $ builtinPrefix ++ "eq"),
  ("/=", Op 4 AssL $ builtinPrefix ++ "neq"),
  ("<=", Op 4 AssL $ builtinPrefix ++ "le"),
  (">=", Op 4 AssL $ builtinPrefix ++ "ge"),
  ("<", Op 4 AssL $ builtinPrefix ++ "lt"),
  (">", Op 4 AssL $ builtinPrefix ++ "gt"),
  ("+", Op 6 AssL $ builtinPrefix ++ "add"),
  ("-", Op 6 AssL $ builtinPrefix ++ "sub"),
  ("*", Op 7 AssL $ builtinPrefix ++ "mul"),
  ("/", Op 7 AssL $ builtinPrefix ++ "div"),
  ("%", Op 7 AssL $ builtinPrefix ++ "mod"),
  ("", Op 10 AssL "__special__lambda")]

applyOp (Op _ _ "__special__lambda") e1 e2 = EApply (takePos e2) e1 e2

applyOp (Op _ _ name) e1 e2 =
    EApply (takePos e2)
      (EApply (takePos e1) (EVar (takePos e1) name) e1)
      e2


exprConversion [e] [] [] = e

exprConversion (h2:h1:t) (o:to) [] = exprConversion (applyOp o h1 h2:t) to []

exprConversion output [] ((o,e):to) = exprConversion (e:output) [o] to

exprConversion
  output  @(h2:h1                 :t)
  ops     @(ho@(Op pr1 _ _)       :to)
  input   @((o@(Op pr2 as _), h3) :ti) =
  if (as == AssL && pr2 <= pr1) || (as == AssR && pr2 < pr1)
    then exprConversion (applyOp ho h1 h2:t) to input
    else exprConversion (h3:output) (o:ops) ti

exprConversion _ _ _ = undefined -- should never happen

finishOps om ex = let
  mapLets = mapM (\(v, t, e) -> finishOps om e >>= \e' -> return (v, t, e'))
  in case ex of
    ELambda p v e   -> do
      e' <- (finishOps om e)
      return $ ELambda p v e'
    ELet p l e      -> do
      l' <- mapLets l
      e' <- finishOps om e
      return $ ELet p l' e'
    ELetRec p l e   -> do
      l' <- mapLets l
      e' <- finishOps om e
      return $ ELetRec p l' e'
    EApply p e1 e2  -> do
      e1' <- (finishOps om e1)
      e2' <- (finishOps om e2)
      return $ EApply p e1' e2'
    EOpExpr _ h t   -> do
      h' <- finishOps om h
      t' <- mapM (\(oname, e) -> do
        e' <- finishOps om e
        case M.lookup oname om of
          Nothing -> fail $ "No such operator: " ++ oname
          Just op -> return (op, e')
        ) t
      return $ exprConversion [h'] [] t'
    EData _ _       -> return ex
    EVar _ _        -> return ex

-- toplevel
namedExp = do
  rword "def"
  var <- lIdentifier
  ann <- tTypeAnnotation
  rop "="
  e <- eExpr
  e' <- finishOps defOpMap e
  return $ NamedExp var ann e'

toplevelDef =
      namedExp

langParser = between sc eof (some toplevelDef)
