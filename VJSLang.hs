import VData
import VParser

type Block
  = VList Stmt

type Identifier 
  = String

data Stmt
  = LiteralStmt Lit
  | FunctionDef Identifier (VList Identifier) Block
  | FunctionCall Identifier (VList Expr)
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
  | VarExpr Identifier
  | BinOpExpr BinOp Expr Expr
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

literal :: VParser Lit
literal = stringLiteral -||- numberLiteral

stmt :: VParser Stmt
stmt  =     returnStmt
      -||-  functionDef 
      -||-  functionCall
      -||-  (fmap LiteralStmt literal)

identifier :: VParser String
identifier = do
  let alpha = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
  let numeric = ['0'..'9']
  x <- oneOf alpha
  xs <- manyL $ oneOf $ alpha ++ numeric
  return $ x : xs

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

functionCall :: VParser Stmt
functionCall = do
  name <- identifier
  whitespace0
  char '('
  args <- expr `sepByV` (whitespace0 >> char ',' >> whitespace0)
  char ')'
  return $ FunctionCall name args

returnStmt :: VParser Stmt
returnStmt = fmap ReturnStmt $ string "return" >> whitespace >> expr

op :: VParser BinOp
op = oneOf "+-*/" >>= \o -> return $ case o of
  '+' -> Add
  '-' -> Sub
  '*' -> Mul
  '/' -> Div

expr :: VParser Expr
expr = mergeContext $ do
  l <-    (fmap LitExpr literal)
    -||-  (fmap VarExpr identifier)
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
