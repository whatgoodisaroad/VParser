import VData
import VParser
import VStringReader

type Block
  = VList Stmt

type Identifier 
  = String

type Reference
  = VList Identifier

type Arguments 
  = VList Identifier

data Stmt
  = ExprStmt Expr

  | VarDefStmt Identifier
  | VarInitStmt Identifier Expr
  | VarAssignStmt Reference Expr

  | FunctionDef Identifier Arguments Block
  
  | ReturnStmt Expr
  | StmtC Dim Stmt Stmt
  deriving (Show, Eq)

instance Variational Stmt where
  mkChoice = StmtC
  select1 s (StmtC d l r) = case (d == dimOf s, s) of
    (False, _) -> StmtC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ stmt = stmt

data Lit
  = NumLit String
  | StringLit String
  | ObjectLit (VList (String, Expr))
  | InlineFunction Arguments Block
  | LitC Dim Lit Lit
  deriving (Show, Eq)

instance Variational Lit where
  mkChoice = LitC
  select1 s (LitC d l r) = case (d == dimOf s, s) of
    (False, _) -> LitC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ lit = lit

data Expr
  = LitExpr Lit
  | VarExpr Reference
  | BinOpExpr BinOp Expr Expr
  | FunctionCall Reference (VList Expr)
  | ExprC Dim Expr Expr
  deriving (Show, Eq)

instance Variational Expr where
  mkChoice = ExprC
  select1 s (ExprC d l r) = case (d == dimOf s, s) of
    (False, _) -> ExprC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ expr = expr

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Dot
  deriving (Show, Eq)

whitespace :: VParser ()
whitespace = (manyL1 $ oneOf " \t\n") >> return ()

whitespace0 :: VParser ()
whitespace0 = (manyL $ oneOf " \t\n") >> return ()

-- Pad a parser with whitespace to the left or right or both:
wpadl, wpadr, wpad :: VParser a -> VParser a
wpadl p = whitespace0 >> p
wpadr p = do { a <- p; whitespace0; return a }
wpad = wpadr . wpadl


numberLiteral :: VParser Lit
numberLiteral = fmap NumLit $ manyL1 $ oneOf ['0'..'9']

stringLiteral :: VParser Lit
stringLiteral = do
  delimiter <- oneOf "\"'"
  contents <- manyL $ sat (/= delimiter)
  char delimiter
  return $ StringLit contents

objectLiteral :: VParser Lit
objectLiteral = do
  char '{'
  kvps <- wpad $ objectKeyValuePair `sepByV` (wpad $ char ',')
  char '}'
  return $ ObjectLit kvps

objectKeyValuePair :: VParser (String, Expr)
objectKeyValuePair = do
  name <- identifier -|- (fmap (\(StringLit x) -> x) stringLiteral)
  wpad $ char ':'
  val <- expr
  return (name, val)

inlineFunction :: VParser Lit
inlineFunction = do
  string "function"
  whitespace0
  char '('
  args <- wpad $ identifier `sepByV` (wpad $ char ',')
  char ')'
  whitespace0
  char '{'
  body <- wpad block
  char '}'
  return $ InlineFunction args body

literal :: VParser Lit
literal =     inlineFunction 
        -||-  objectLiteral 
        -||-  stringLiteral 
        -||-  numberLiteral

varDef :: VParser Stmt
varDef = do
  string "var"
  whitespace
  name <- identifier
  return $ VarDefStmt name

varInit :: VParser Stmt
varInit = do
  string "var"
  whitespace
  name <- identifier
  wpad $ char '='
  val <- expr
  return $ VarInitStmt name val

varAssign :: VParser Stmt
varAssign = do
  name <- reference
  wpad $ char '='
  val <- expr
  return $ VarAssignStmt name val

stmt :: VParser Stmt
stmt  =     returnStmt
      -||-  varInit
      -||-  varDef
      -||-  varAssign
      -||-  functionDef 
      -||-  (fmap ExprStmt expr)

identifier :: VParser String
identifier = do
  let alpha = ['A'..'Z'] ++ ['a'..'z'] ++ "$_"
  let numeric = ['0'..'9']
  x <- oneOf alpha
  xs <- manyL $ oneOf $ alpha ++ numeric
  return $ x : xs

reference :: VParser Reference
reference = identifier `sepBy1V` (char '.')

block :: VParser Block
block = mergeContext $ manyV $ do
  s <- stmt
  char ';'
  whitespace0
  return s

functionDef :: VParser Stmt
functionDef = do
  string "function"
  whitespace
  name <- identifier
  char '('
  argNames <- sepByV identifier (whitespace0 >> string "," >> whitespace0)
  char ')'
  whitespace0
  char '{'
  whitespace0
  content <- block
  char '}'
  return $ FunctionDef name argNames content

functionCall :: VParser Expr
functionCall = do
  name <- reference
  whitespace0
  char '('
  args <- expr `sepByV` (whitespace0 >> char ',' >> whitespace0)
  char ')'
  return $ FunctionCall name args

returnStmt :: VParser Stmt
returnStmt = fmap ReturnStmt $ string "return" >> whitespace >> expr

op :: VParser BinOp
op = oneOf ".+-*/" >>= \o -> return $ case o of
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div
  '.' -> Dot

expr :: VParser Expr
expr = mergeContext $ do
  l <-
          (fmap LitExpr literal)
    -||-  functionCall   
    -||-  (fmap VarExpr reference)
    -||-  parenExpr
  binOp' l
  where
    binOp', binOp'' :: Expr -> VParser Expr
    binOp' l = binOp'' l -||- return l
    binOp'' l = do
      whitespace0
      oper <- op
      whitespace0
      r <- expr
      return $ BinOpExpr oper l r

    parenExpr :: VParser Expr
    parenExpr = mergeContext $ do
      char '('
      whitespace0
      e <- expr
      whitespace0
      char ')'
      return e
