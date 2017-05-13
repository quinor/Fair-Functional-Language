module Interpreter.Parser {-(
  parse_str,
  parse_file
)-} where
import Interpreter.Defs
import System.IO
import Data.Maybe
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as Map

import Debug.Trace

type ParseResult = Either (ParseError (Token String) Dec) [TLD]

--parse program from string
parse_str :: String -> ParseResult

--parse program from file using IO monad
parse_file :: String -> IO ParseResult



parser :: Parser [TLD]

data Ass = AssL | AssR deriving (Eq, Show)

data Op = Op Int Ass VarE deriving Show -- precedence, associativity, function name


parse_str s = parse parser "<none>" s

parse_file f = do
  raw <- readFile f
  return $ parse_str raw

parser = between sc eof (some toplevel_def)



builtin = "__builtin__"

op_map :: Map.Map String Op
op_map = Map.fromList [
  ("$", Op 0 AssR ("__special__lambda")),
  ("+", Op 6 AssL (builtin ++ "add")),
  ("-", Op 6 AssL (builtin ++ "sub")),
  ("*", Op 7 AssL (builtin ++ "mul")),
  ("/", Op 7 AssL (builtin ++ "div")),
  ("%", Op 7 AssL (builtin ++ "mod")),
  ("", Op 10 AssL ("__special__lambda"))]



reserved :: [String]
sc :: Parser ()
rword :: String -> Parser ()
rop :: String -> Parser ()
lexeme :: Parser a -> Parser a
symbol :: String -> Parser String
parens :: Parser a -> Parser a
integer :: Parser Int
boolean :: Parser Bool
l_identifier :: Parser String
u_identifier :: Parser String
operator :: Parser Op
expr :: Parser Exp
expr_single :: Parser Exp
e_lambda :: Parser Exp
e_let :: Parser Exp
e_rec :: Parser Exp
apply_op :: Op -> Exp -> Exp -> Exp
expr_conversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
named_exp :: Parser (VarE, Exp)
toplevel_def :: Parser TLD

reserved = ["def", "data", "type", "True", "False", "let", "rec", "match", "with", "in",
  "\\", "->", "="]

sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "{#" "#}")

rword s = try $ string s *> notFollowedBy (alphaNumChar <|> (char '_')) *> sc

rop s = try $ string s *> notFollowedBy (oneOf "=-.:/\\~<>!@$%^&*()+{}|?") *> sc

lexeme = L.lexeme sc

symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")

integer = fromInteger <$> lexeme L.integer

boolean = (rword "True" *> pure True) <|> (rword "False" *> pure False)

l_identifier = try $ do
  s <- lexeme $ (:) <$> lowerChar <*> many (alphaNumChar <|> (char '_'))
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ "cannot be an identifier"
    else return s

u_identifier = try $ do
  s <- lexeme $ (:) <$> upperChar <*> many (alphaNumChar <|> (char '_'))
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ "cannot be an identifier"
    else return s

operator = try $ do
  s <- lexeme $ many (oneOf "=-.:/\\~<>!@$%^&*()+{}|?") -- reserved: [](),_"'`
  if s `elem` reserved
    then fail $ "forbidden: keyword " ++ show s ++ "cannot be an identifier"
    else return $ fromJust $ Map.lookup s op_map

expr = do
  h <- expr_single
  t <- many (try $ (,) <$> operator <*> expr_single)
  return $ expr_conversion [h] [] t -- TODO: stack expr parsing algorithm

expr_single =
      (EData . DBool) <$> boolean
  <|> (EData . DInt) <$> integer
  <|> EVar <$> l_identifier
  <|> e_lambda
  <|> e_let
  <|> e_rec
  <|> parens expr

e_lambda = do
  rop "\\"
  var <- l_identifier
  rop "->"
  e <- expr
  return $ ELambda var e

e_let = do
  rword "let"
  var <- l_identifier
  rop "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ ELet var e1 e2

e_rec = do
  rword "rec"
  var <- l_identifier
  rop "="
  e1 <- expr
  rword "in"
  e2 <- expr
  return $ ELetRec var e1 e2


apply_op (Op _ _ "__special__lambda") e1 e2 = EApply e1 e2

apply_op (Op _ _ name) e1 e2 = EApply (EApply (EVar name) e1) e2


expr_conversion [e] [] [] = e

expr_conversion (h2:h1:t) (o:to) [] = expr_conversion ((apply_op o h1 h2):t) to []

expr_conversion output [] ((o,e):to) = expr_conversion (e:output) [o] to

expr_conversion
  output  @(h2:h1                 :t)
  ops     @(ho@(Op pr1 _ _)       :to)
  input   @((o@(Op pr2 as _), h3) :ti) =
  if (as == AssL && pr2 <= pr1) || (as == AssR && pr2 < pr1)
    then expr_conversion ((apply_op ho h1 h2):t) to input
    else expr_conversion (h3:output) (o:ops) ti

expr_conversion _ _ _ = undefined -- should never happen


named_exp = do
  rword "def"
  var <- l_identifier
  rop "="
  e <- expr
  return (var, e)

toplevel_def =
      (uncurry NamedExp) <$> named_exp