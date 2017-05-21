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


data Ass = AssL | AssR deriving (Eq, Show)

data Op = Op Int Ass VarE deriving Show -- precedence, associativity, function name




opMap :: M.Map String Op
opMap = M.fromList [
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



reserved :: [String]
getPos :: Parser Position
sc :: Parser ()
rword :: String -> Parser ()
rop :: String -> Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
parens :: Parser a -> Parser a
eInteger :: Parser Exp
eBoolean :: Parser Exp
eVariable :: Parser Exp
lIdentifier :: Parser String
uIdentifier :: Parser String
operator :: Parser Op
expr :: Parser Exp
exprSingle :: Parser Exp
eLambda :: Parser Exp
eLet :: Parser Exp
eRec :: Parser Exp
eIf :: Parser Exp
applyOp :: Op -> Exp -> Exp -> Exp
exprConversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
namedExp :: Parser (VarE, Exp)
toplevelDef :: Parser TLD

reserved = ["def", "data", "type", "True", "False", "let", "rec", "and", "match", "with", "in", "if",
  "then", "else", "\\", "->", "="]

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

operator = try $ do
  s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*+{}|?") -- reserved: [](),_"'`
  if s `elem` reserved
    then fail $ "forbidden keyword symbol: " ++ show s ++ " cannot be an operator"
    else case M.lookup s opMap of
      Just x  -> return x
      Nothing -> fail $ "no such operator: " ++ show s

expr = do
  h <- exprSingle
  t <- many (try $ (,) <$> operator <*> exprSingle)
  return $ exprConversion [h] [] t -- TODO: custom operators

exprSingle =
      eBoolean
  <|> eInteger
  <|> eVariable
  <|> eLambda
  <|> eLet
  <|> eRec
  <|> eIf
  <|> parens expr


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
  e <- expr
  return $ foldr (ELambda p) e vars

letDecls :: Parser [(VarE, Exp)]
letDecls = (\x -> sepBy1 x (rword "and")) $ do
  v <- lIdentifier
  rop "="
  e <- expr
  return (v, e)

eLet = do
  p <- getPos
  rword "let"
  vars <- letDecls
  rword "in"
  e2 <- expr
  return $ ELet p vars e2

eRec = do
  p <- getPos
  rword "rec"
  vars <- letDecls
  rword "in"
  e2 <- expr
  return $ orderRecClauses p vars e2

eIf = do
  p0 <- getPos
  rword "if"
  p1 <- getPos
  cond <- expr
  rword "then"
  p2 <- getPos
  e1 <- expr
  rword "else"
  p3 <- getPos
  e2 <- expr
  return $ EApply p3 (EApply p2 (EApply p1 (EVar p0 $ builtinPrefix ++ "if") cond) e1) e2

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


getVars :: Exp -> S.Set VarE
getVars ex = case ex of
  EVar _ v        -> S.singleton v
  EData _ _       -> S.empty
  EApply _ e1 e2  -> getVars e1 `S.union` getVars e2
  ELet _ l e      -> (S.unions $ map (getVars . snd) l) `S.union` (getVars e S.\\ (S.fromList $ map fst l))
  ELetRec _ l e   -> (getVars e `S.union` (S.unions $ map (getVars . snd) l)) S.\\ (S.fromList $ map fst l)
  ELambda _ n e   -> getVars e S.\\ S.singleton n

orderRecClauses :: Position -> [(VarE, Exp)] -> Exp -> Exp
orderRecClauses p l ex = let
  labels = map fst l
  gr = map (\(n, e) -> ((n, e), n, filter (\a -> S.member a $ getVars e) labels)) l
  scc = map G.flattenSCC $ G.stronglyConnComp gr
  in foldr (ELetRec p) ex scc


namedExp = do
  rword "def"
  var <- lIdentifier
  rop "="
  e <- expr
  return (var, e)

toplevelDef =
      uncurry NamedExp <$> namedExp

langParser = between sc eof (some toplevelDef)
