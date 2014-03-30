import VData
import VParser
import VStringReader

type Block
  = VList (Either Stmt CtrlStruct)

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
  | WeakEq
  | StrongEq
  | WeakNotEq
  | StrongNotEq
  | LessThan
  | LessThanEq
  | GreaterThan
  | GreaterThanEq
  deriving (Show, Eq)

data CtrlStruct
  = IfStruct Expr Block
  | IfElseStruct Expr Block Block
  | WhileStruct Expr Block
  | DoWhileStruct Expr Block
  | ForStruct Stmt Expr Stmt Block
  | FunctionDef Identifier Arguments Block
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
stmt  = do
  s <-    returnStmt
    -||-  varInit
    -||-  varDef
    -||-  varAssign
    -||-  (fmap ExprStmt expr)
  char ';'
  return s

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
  s <- (stmt >>= return . Left) -|- (ctrlStruct >>= return . Right)
  whitespace0
  return s

functionCall :: VParser Expr
functionCall = do
  name <- reference
  whitespace0
  char '('
  args <- wpad $ expr `sepByV` (whitespace0 >> char ',' >> whitespace0)
  char ')'
  return $ FunctionCall name args

returnStmt :: VParser Stmt
returnStmt = fmap ReturnStmt $ string "return" >> whitespace >> expr

op :: VParser BinOp
op  = foldr1 (-|-)
    $ map (\(s, v) -> string s >> return v )
    $ [ 
      ("+", Add), 
      ("-", Sub), 
      ("*", Mul), 
      ("/", Div), 
      (".", Dot), 
      ("==", WeakEq),
      ("===", StrongEq),
      ("!=", WeakNotEq),
      ("!==", StrongNotEq),
      ("<", LessThan),
      ("<=", LessThanEq),
      (">", GreaterThan),
      (">=", GreaterThanEq)
    ]

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


ctrlStruct :: VParser CtrlStruct
ctrlStruct  =   functionDef
            -|- ifElseStruct
            -|- ifStruct
            -|- doWhileStruct
            -|- whileStruct
            -|- forStruct

functionDef :: VParser CtrlStruct
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

structBody :: VParser Block
structBody = do
  let blockBody = do { char '{'; b <- wpad block; char '}'; return b; }
  let stmtBody = stmt >>= return . return . Left
  let ctrlBody = ctrlStruct >>= return . return . Right
  blockBody -|- stmtBody -|- ctrlBody

ifElseStruct :: VParser CtrlStruct
ifElseStruct = do
  string "if"
  whitespace0
  char '('
  e <- wpad expr
  char ')'
  whitespace0
  b1 <- structBody
  wpad $ string "else"
  b2 <- structBody
  return $ IfElseStruct e b1 b2

ifStruct :: VParser CtrlStruct
ifStruct = do
  string "if"
  whitespace0
  char '('
  e <- wpad expr
  char ')'
  whitespace0
  b <- structBody
  return $ IfStruct e b

whileStruct :: VParser CtrlStruct
whileStruct = do
  string "while"
  whitespace0
  char '('
  e <- expr
  char ')'
  whitespace0
  b <- structBody
  return $ WhileStruct e b

doWhileStruct :: VParser CtrlStruct
doWhileStruct = do
  string "do"
  whitespace0
  b <- structBody
  whitespace0
  string "while"
  whitespace0
  char '('
  e <- expr
  string ");"
  return $ DoWhileStruct e b

forStruct :: VParser CtrlStruct
forStruct = do
  string "for"
  whitespace0
  char '('
  c1 <- varInit
  char ';'
  whitespace0
  c2 <- expr
  char ';'
  whitespace0
  c3 <- varAssign
  char ')'
  whitespace0
  b <- structBody
  return $ ForStruct c1 c2 c3 b
