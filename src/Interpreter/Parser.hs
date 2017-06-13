{-# Options -Wall #-}

module Interpreter.Parser (
  parseStr,
  ParseResult
) where
import Interpreter.Defs
import Interpreter.Eval
import Interpreter.Primitives (builtinPrefix)
import Control.Monad
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Graph as G
import Data.Maybe (fromJust)




type ParseResult = Either (ParseError (Token String) Dec) Program

--parse program from string
parseStr :: String -> String -> ParseResult
parseStr = parse langParser

--parser of the entire language
langParser :: Parser Program


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
unique :: (Show a, Ord a) => [a] -> Parser ()
letDecls :: Parser [(VarE, Maybe Type, Exp)]
eLet :: Parser Exp
eRec :: Parser Exp
eIf :: Parser Exp
matchClause :: Parser (String, Int, Exp)
eMatch :: Parser Exp
eMatchFun :: Parser Exp

--the exp is ELetRec
orderRecClauses :: Exp -> Exp
getVars :: Exp -> S.Set VarE


-- type parsing
tInt :: Parser Type
tBool :: Parser Type
tLambda :: Parser Type
tAlgebraic :: Parser Type
tVar :: Parser Type
tTypeSingle :: Parser Type
tType :: Parser Type
tTypeAnnotation :: Parser (Maybe Type)

-- expr postproc
defOpMap :: OpMap
applyOp :: Op -> Exp -> Exp -> Exp
exprConversion :: [Exp] -> [Op] -> [(Op, Exp)] -> Exp
postprocessProgram ::
  OpMap ->
  (M.Map String [String]) ->
  (M.Map String (Int, String)) ->
  Exp -> Parser Exp


-- algtype parsing
singleConstructor :: Parser (String, [Type])

-- toplevel parsing
newOperator :: Parser TLD
allVars :: Type -> S.Set String
newAlgType :: Parser TLD
namedExp :: Parser TLD
toplevelDef :: Parser TLD



-- utility/defs
reserved = ["def", "data", "type", "True", "False", "let", "rec", "and", "match", "with", "in", "if",
  "then", "else", "\\", "->", "<-", "=", "|", "Integer", "Boolean", "newop", "of", "Int", "Bool", "end"]

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
  s <- lexeme $ (:) <$> (lowerChar <|> char '_') <*> many (alphaNumChar <|> char '_')
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
  <|> eMatch
  <|> eMatchFun
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
  ident <- lIdentifier <|> uIdentifier
  return $ EVar p ident

eLambda = do
  p <- getPos
  vars <- try (rop "\\" *> some lIdentifier)
  rop "->"
  e <- eExpr
  return $ foldr (ELambda p) e vars

unique l = foldM_ (\s n ->
    if n `S.member` s
      then fail $ "symbol " ++ show n ++ " appears more than once in the declaration!"
      else return $ n `S.insert` s
  )
  S.empty
  l

letDecls = do
  result <- (\x -> sepBy1 x (rword "and")) $ do
    v <- lIdentifier
    ann <- tTypeAnnotation
    rop "="
    e <- eExpr
    return (v, ann, e)
  unique $ map (\(a, _, _) -> a) result
  return result


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
  return $ orderRecClauses $ ELetRec p vars e2


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


matchClause = do
  p <- getPos
  cName <- uIdentifier
  vars <- many lIdentifier
  rop "->"
  e <- eExpr
  return $ (cName, length vars, foldr (ELambda p) e vars)


eMatch = do
  p <- getPos
  rword "match"
  e <- eExpr
  rword "with"
  decls <- sepBy1 matchClause (rop "|")
  _ <- optional $ rword "end"
  return $ EMatch p decls e


eMatchFun = do
  p <- getPos
  try (rop "\\" >> rword "match")
  decls <- sepBy1 matchClause (rop "|")
  _ <- optional $ rword "end"
  let varName = builtinPrefix ++ "match_var"
  return $ ELambda p varName $ EMatch p decls (EVar p varName)


-- get all variables appearing in an expression that were not defined in it,
-- for ordering rec clauses in orderRecClauses
getVars ex = case ex of
  EVar _ v        -> S.singleton v
  EData _ _       -> S.empty
  EApply _ e1 e2  -> getVars e1 `S.union` getVars e2
  ELet _ l e      ->
    (S.unions $ map (getVars . \(_,_,a) -> a)l) `S.union`
    (getVars e S.\\ (S.fromList $ map (\(a,_,_) -> a) l))
  ELetRec _ l e   ->
    (getVars e `S.union` (S.unions $ map (getVars . (\(_,_,a) -> a)) l)) S.\\
    (S.fromList $ map (\(a,_,_) -> a) l)
  ELambda _ n e   -> getVars e S.\\ S.singleton n
  EOpExpr _ e l   -> getVars e `S.union` S.unions (map (\(_,eu) -> getVars eu) l)
  EMatch _ l e    -> getVars e `S.union` S.unions (map (\(_,_,eu) -> getVars eu) l)

orderRecClauses (ELetRec p l ex) = let
  labels = map (\(a,_,_) -> a) l
  gr = map (\(n, ann, e) -> ((n, ann, e), n, filter (\a -> S.member a $ getVars e) labels)) l
  scc = map G.flattenSCC $ G.stronglyConnComp gr
  in foldr (ELetRec p) ex scc

orderRecClauses _ = undefined -- I _know_ it won't be called on anything else.


-- type
tInt = do
  rword "Int"
  return TInt

tBool = do
  rword "Bool"
  return TBool

tLambda = try $ do -- I regret that try but it has to be there...
  t1 <- tTypeSingle
  rop "->"
  t2 <- tType
  return $ TLambda t1 t2

tAlgebraic = do
  name <- uIdentifier
  ts <- many tType
  return $ TAlgebraic name ts

tVar = do
  a <- lIdentifier
  return $ TUserVar a

tTypeSingle = do
      tInt
  <|> tBool
  <|> tAlgebraic
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
  ("||", Op 2 AssR $ builtinPrefix ++ "or"),
  ("&&", Op 3 AssR $ builtinPrefix ++ "and"),
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

postprocessProgram opMap typeMap constructorMap prog = let
  mapLets = mapM (\(v, t, e) -> postprocImpl e >>= \e' -> return (v, t, e'))
  postprocImpl = \ex -> case ex of
    ELambda p v e   -> do
      e' <- (postprocImpl e)
      return $ ELambda p v e'
    ELet p l e      -> do
      l' <- mapLets l
      e' <- postprocImpl e
      return $ ELet p l' e'
    ELetRec p l e   -> do
      l' <- mapLets l
      e' <- postprocImpl e
      return $ ELetRec p l' e'
    EApply p e1 e2  -> do
      e1' <- (postprocImpl e1)
      e2' <- (postprocImpl e2)
      return $ EApply p e1' e2'
    EOpExpr p h t   -> do
      h' <- postprocImpl h
      t' <- mapM (\(oname, e) -> do
        e' <- postprocImpl e
        case M.lookup oname opMap of
          Nothing -> fail $ "No such operator: " ++ oname ++ " (actual position " ++ show p ++ ") "
          Just op -> return (op, e')
        ) t
      return $ exprConversion [h'] [] t'
    EMatch p clauses e    -> do
      e' <- postprocImpl e
      clauses' <- mapM (\(n, a, f) -> postprocImpl f >>= \f' -> return (n, a, f')) clauses
      tmp <- mapM (\(name,arity,_) ->
        case M.lookup name constructorMap of
          Nothing       -> fail $ "No such constructor: " ++ name ++ " in " ++ show p
          Just (ar, ty) -> return (ar == arity, ty)
        ) clauses'

      -- arity check
      unless (and $ map fst tmp) $ fail $ "Incorrect arity of constructor in " ++ show p
      --types check
      unless
        ((1 ==) . length . L.nub $ map snd tmp) $
        fail $ "Constructors are not of the same type in " ++ show p

      let typeName = snd $ head tmp
      let ctors = tail $ fromJust $ M.lookup typeName typeMap -- tail for destructor removal

      --clause uniqueness check
      unique (map (\(n,_,_) -> n) clauses')
      --exhaustiveness check
      unless (length clauses' == length ctors) $ fail $ "Pattern is not exhaustive in " ++ show p
      let {orderedFuns = let
        dict = M.fromList $ map (\(n,_,f) -> (n, f)) clauses'
        in map (\name -> fromJust $ M.lookup name dict) ctors
      }

      let typeFn = EVar p $ builtinPrefix ++ "match_" ++ typeName
      let matchExpr = foldr (flip $ EApply p) typeFn (e': reverse orderedFuns)

      return matchExpr

    EData _ _       -> return ex
    EVar _ _        -> return ex
  in postprocImpl prog


-- algtype
singleConstructor = do
  name <- uIdentifier
  paramTypes <- many tType
  return (name, paramTypes)


-- toplevel
newOperator = do
  rword "newop"
  sign <- operator
  rword "of"
  ass <- (AssL <$ rword "Left") <|> (AssR <$ rword "Right")
  prec <- fromInteger <$> lexeme L.integer
  name <- lIdentifier
  return $ Operator sign $ Op prec ass name


allVars t = case t of
  TUserVar x      -> S.singleton x
  TLambda t1 t2   -> S.union (allVars t1) (allVars t2)
  TAlgebraic _ ts -> S.unions (map allVars ts)
  _               -> S.empty

newAlgType = do
  rword "data"
  name <- uIdentifier
  params <- many lIdentifier
  rop "="
  constructors <- sepBy1 singleConstructor (rop "|")
  unless
    (null $
      (S.unions $ map allVars $ concatMap snd constructors) S.\\
      (S.fromList params)
    )
    (fail "undefined type variables in constructor definition!")
  return $ let
    retTypeConstructor = TAlgebraic name $ map TVar params
    primConstructors = map
      (\(cName, argTypes) -> let
        ftype = removeUser $ foldr TLambda retTypeConstructor argTypes
        cfun = (\d _ -> return $ DAlgebraic cName d)
        in Prim cName ftype (length argTypes) cfun
        )
      constructors
    retTypeMatch = TVar "_match_ret_type"

    typeMatchClauses = map (\(_, argTypes) -> foldr TLambda retTypeMatch argTypes) constructors
    typeMatch = removeUser $ foldr TLambda (TLambda retTypeConstructor retTypeMatch) typeMatchClauses
    dFun = \args st -> do
      let funs = init args
      DAlgebraic cName vals <- computeData st (last args)
      return $ foldr
        (\(f, n) follow -> if n == cName then foldl (DLazyApply st) f vals else follow)
        DUndefined -- should always match something
        (zip funs $ map fst constructors)

    destructor = Prim (builtinPrefix ++ "match_" ++ name) typeMatch (1 + length constructors) dFun

    in AlgType name (1 + length params) (destructor:primConstructors)

namedExp = do
  rword "def"
  var <- lIdentifier
  ann <- tTypeAnnotation
  rop "="
  e <- eExpr
  return $ NamedExp var ann e

toplevelDef =
      namedExp
  <|> newOperator
  <|> newAlgType

langParser = do
  p0 <- getPos
  tlds <- between sc eof (some toplevelDef)
  p1 <- getPos

  --operators
  let opMap = M.union (M.fromList [(s,op) | (Operator s op) <- tlds]) defOpMap

  --algtypes
  let algTypes = [(name, arity, consPrimitives) | (AlgType name arity consPrimitives) <- tlds]
  unique $ map (\(n,_,_) -> n) algTypes
  unique $ concatMap (\(_,_,a) -> map takeName a) algTypes
  let {typeDefinitions = let
    constructorEList = map
      (\p -> (takeName p, Nothing, EData (Position "ADTs" 0 0) (DPrimitive p)))
      (concatMap (\(_, _, p) -> p) algTypes)
    in ELet (Position "ADTs" 0 0) constructorEList
  }
  let typeConstructors = M.fromList $ map (\(n, _, cs) -> (n, map takeName cs)) algTypes
  let {constructorTypes = M.fromList $ concatMap
      (\(n, _, cs) -> map (\(Prim cName _ ar _) -> (cName, (ar, n))) cs)
      algTypes
  }
  --definitions
  let defs = [(n,ann,e) | (NamedExp n ann e) <- tlds]
  unique $ map (\(a,_,_) -> a) defs

  --combining stuff together

  let prog = ELetRec p0 defs (EVar p1 "main")
  prog' <- postprocessProgram opMap typeConstructors constructorTypes prog
  let prog'' = orderRecClauses prog'
  let prog''' = typeDefinitions prog''
  return $ Program prog'''
