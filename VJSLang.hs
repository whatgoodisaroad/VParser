import VData
import VParser
import VStringReader

data Code
  = Stmt Stmt
  | CtrlStruct CtrlStruct
  | Comment Comment
  | CodeChc Dim Code Code
  deriving (Show, Eq)

instance Variational Code where
  mkChoice = CodeChc
  select1 s (CodeChc d l r) = case (d == dimOf s, s) of
    (False, _) -> mkChoice d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (Stmt st) = Stmt $ select1 s st
  select1 s (CtrlStruct st) = CtrlStruct $ select1 s st
  select1 s (Comment c) = Comment $ select1 s c

data Block
  = Block (VList Code)
  deriving (Show, Eq)

instance Variational Block where
  mkChoice d (Block l) (Block r) = Block $ mkChoice d l r
  select1 s (Block c) = Block $ select1' s c

type Identifier 
  = String

data RefPart
  = RefId Identifier
  | RefExpr Expr
  deriving (Show, Eq)

data Reference
  = Reference (VList RefPart)
  deriving (Show, Eq)

instance Variational Reference where
  mkChoice d (Reference l) (Reference r) = Reference $ mkChoice d l r
  select1 s (Reference ref) = Reference $ select1 s $ fmap f ref
    where
      f (RefId i) = RefId i
      f (RefExpr e) = RefExpr $ select1 s e

data Arguments 
  = Args (VList Identifier)
  deriving (Show, Eq)

instance Variational Arguments where
  mkChoice d (Args l) (Args r) = Args $ mkChoice d l r
  select1 s (Args as) = Args $ select1 s as

data Var
  = VarDef Identifier
  | VarInit Identifier Expr
  | VarChc Dim Var Var
  deriving (Show, Eq)

instance Variational Var where
  mkChoice = VarChc
  select1 s (VarChc d l r) = case (d == dimOf s, s) of
    (False, _) -> mkChoice d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (VarInit i e) = VarInit i (select1 s e)
  select1 _ v = v

data Stmt
  = ExprStmt Expr
  | VarAssignStmt Reference Expr
  | VarStmt (VList Var)
  | ReturnStmt Expr
  | StmtC Dim Stmt Stmt
  deriving (Show, Eq)

instance Variational Stmt where
  mkChoice = StmtC
  select1 s (StmtC d l r) = case (d == dimOf s, s) of
    (False, _) -> StmtC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (ExprStmt e) = ExprStmt $ select1 s e
  select1 s (VarAssignStmt r e) = VarAssignStmt (select1 s r) (select1 s e)
  select1 s (VarStmt l) = VarStmt $ select1' s l
  select1 s (ReturnStmt e) = ReturnStmt $ select1 s e

data Comment 
  = InlineComment String
  | MultiLineComment String
  | CommentChc Dim Comment Comment
  deriving (Show, Eq)

instance Variational Comment where
  mkChoice = CommentChc
  select1 s (CommentChc d l r) = case (d == dimOf s, s) of
    (False, _) -> CommentChc d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ c = c

data Lit
  = NumLit String
  | StringLit String
  | ObjectLit (VList (String, Expr))
  | InlineFunction Arguments Block
  | ArrayLit (VList Expr)
  | LitC Dim Lit Lit
  deriving (Show, Eq)

instance Variational Lit where
  mkChoice = LitC
  select1 s (LitC d l r) = case (d == dimOf s, s) of
    (False, _) -> LitC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (ObjectLit ds) = ObjectLit $ fmap f $ select1 s ds
    where f (k, v) = (k, select1 s v)
  select1 s (InlineFunction a b) = InlineFunction (select1 s a) (select1 s b)
  select1 s (ArrayLit es) = ArrayLit $ select1' s es
  select1 _ lit = lit

data Expr
  = LitExpr Lit
  | VarExpr Reference
  | UnaOpExpr UnaOp Expr
  | BinOpExpr BinOp Expr Expr
  | TernaryExpr Expr Expr Expr
  | FunctionCall Reference (VList Expr)
  | ExprC Dim Expr Expr
  deriving (Show, Eq)

instance Variational Expr where
  mkChoice = ExprC
  select1 s (ExprC d l r) = case (d == dimOf s, s) of
    (False, _) -> ExprC d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (LitExpr l) = LitExpr $ select1 s l
  select1 s (VarExpr r) = VarExpr $ select1 s r
  select1 s (BinOpExpr op e1 e2) = BinOpExpr op (select1 s e1) (select1 s e2)
  select1 s (TernaryExpr c e1 e2) = 
    TernaryExpr (select1 s c) (select1 s e1) (select1 s e2)
  select1 s (FunctionCall r a) = FunctionCall (select1 s r) (select1' s a)

data UnaOp
  = Not
  | Negative
  | IncrPre
  | DecrPre
  | IncrPost
  | DecrPost
  | UnaOpChc Dim UnaOp UnaOp
  deriving (Show, Eq)

instance Variational UnaOp where
  mkChoice = UnaOpChc
  select1 s (UnaOpChc d l r) = case (d == dimOf s, s) of
    (False, _) -> mkChoice d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ b = b

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
  | LogicalOr
  | LogicalAnd
  | BinOpChc Dim BinOp BinOp
  deriving (Show, Eq)

instance Variational BinOp where
  mkChoice = BinOpChc
  select1 s (BinOpChc d l r) = case (d == dimOf s, s) of
    (False, _) -> mkChoice d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ b = b

data CtrlStruct
  = IfStruct Expr Block
  | IfElseStruct Expr Block Block
  | WhileStruct Expr Block
  | DoWhileStruct Expr Block
  | ForStruct Stmt Expr Stmt Block
  | FunctionDef Identifier Arguments Block
  | CtrlStructChc Dim CtrlStruct CtrlStruct
  deriving (Show, Eq)

instance Variational CtrlStruct where
  mkChoice = CtrlStructChc
  select1 s (CtrlStructChc d l r) = case (d == dimOf s, s) of
    (False, _) -> CtrlStructChc d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (IfStruct e b) = IfStruct (select1 s e) (select1 s b)
  select1 s (IfElseStruct e b1 b2) = 
    IfElseStruct (select1 s e) (select1 s b1) (select1 s b2)
  select1 s (WhileStruct e b) = WhileStruct (select1 s e) (select1 s b)
  select1 s (DoWhileStruct e b) = DoWhileStruct (select1 s e) (select1 s b)
  select1 s (ForStruct s1 e s2 b) = 
    ForStruct (select1 s s1) (select1 s e) (select1 s s2) (select1 s b)
  select1 s (FunctionDef i a b) = FunctionDef i (select1 s a) (select1 s b)

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
numberLiteral = mergeResult $ do
  let digits = manyL1 $ oneOf ['0'..'9']
  whole <- digits
  frac <- (char '.' >> digits) -|- return []
  return $ NumLit $ whole ++ "." ++ frac

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

arrayLit ::VParser Lit
arrayLit = do
  char '['
  es <- wpad $ expr `sepByV` (wpad $ char ',')
  char ']'
  return $ ArrayLit es

inlineFunction :: VParser Lit
inlineFunction = do
  string "function"
  whitespace0
  char '('
  args <- argNames
  char ')'
  whitespace0
  char '{'
  body <- wpad block
  char '}'
  return $ InlineFunction args body

literal :: VParser Lit
literal =     inlineFunction 
        -||-  objectLiteral 
        -||-  arrayLit
        -||-  stringLiteral 
        -||-  numberLiteral


multiVarDef :: VParser Stmt
multiVarDef = do
  string "var"
  whitespace
  let vinit = do {
    n <- identifier;
    wpad $ char '=';
    v <- expr;
    return $ VarInit n v;
  }
  let vdef = identifier >>= return . VarDef
  vs <- (vinit -||- vdef) `sepByV` (wpad $ char ',')
  return $ VarStmt vs

varAssign :: VParser Stmt
varAssign = do
  name <- reference
  wpad $ char '='
  val <- expr
  return $ VarAssignStmt name val

stmt :: VParser Stmt
stmt  = do
  s <-    returnStmt
    -||-  multiVarDef
    -||-  varAssign
    -||-  (fmap ExprStmt expr)
  whitespace0
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
reference = mergeResult $ do
  x <- fmap RefId identifier
  xs <- manyV $ 
        (char '.' >> fmap RefId identifier)
    -|- do { char '['; e <- expr; char ']'; return $ RefExpr e }
  return $ Reference $ x `consVL` xs

block :: VParser Block
block = mergeResult $ fmap Block $ manyV $ do
  s <-  (stmt >>= return . Stmt) 
    -|- (ctrlStruct >>= return . CtrlStruct)
    -|- (comment >>= return . Comment)
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

binOp :: VParser BinOp
binOp = foldr1 (-||-)
    $ map (\(s, v) -> string s >> return v )
    $ [ 
      ("+", Add), 
      ("-", Sub), 
      ("*", Mul), 
      ("/", Div), 
      ("===", StrongEq),
      ("==", WeakEq),
      ("!==", StrongNotEq),
      ("!=", WeakNotEq),
      ("<=", LessThanEq),
      ("<", LessThan),
      (">=", GreaterThanEq),
      (">", GreaterThan),
      ("||", LogicalOr),
      ("&&", LogicalAnd)
    ]

unOpPre :: VParser UnaOp
unOpPre = foldr1 (-||-)
    $ map (\(s, v) -> string s >> return v )
    $ [ 
      ("!", Not),
      ("-", Negative),
      ("++", IncrPre),
      ("--", DecrPre)
    ]

unOpPost :: VParser UnaOp
unOpPost = foldr1 (-||-)
    $ map (\(s, v) -> string s >> return v )
    $ [ 
      ("++", IncrPost),
      ("--", DecrPost)
    ]

expr :: VParser Expr
expr = mergeResult $ do
  e <-    
          prefix
    -||-  (fmap LitExpr literal)
    -||-  functionCall
    -||-  (fmap VarExpr reference)
    -||-  parenExpr

  p <- postfix e -||- return e

  ternary p -||- binary p -||- return p
    where

      prefix = mergeResult $ do
        o <- wpad unOpPre
        e <- wpad expr
        return $ UnaOpExpr o e

      postfix e = do
        o <- wpad unOpPost
        return $ UnaOpExpr o e

      binary l = do
        whitespace0
        oper <- binOp
        whitespace0
        r <- expr
        return $ BinOpExpr oper l r

      ternary c = do
        wpad $ char '?'
        e1 <- expr
        wpad $ char ':'
        e2 <- expr
        return $ TernaryExpr c e1 e2

      parenExpr = mergeResult $ do
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

argNames :: VParser Arguments
argNames = 
    mergeResult 
  $ fmap Args 
  $ identifier `sepByV` (wpad $ string ",")

functionDef :: VParser CtrlStruct
functionDef = mergeResult $ do
  string "function"
  whitespace
  name <- identifier
  char '('
  args <- argNames
  char ')'
  whitespace0
  char '{'
  whitespace0
  content <- block
  char '}'
  return $ FunctionDef name args content

structBody :: VParser Block
structBody = do
  let blockBody = do { char '{'; b <- wpad block; char '}'; return b; }
  let stmtBody = stmt >>= return . Block . return . Stmt
  let ctrlBody = ctrlStruct >>= return . Block . return . CtrlStruct
  blockBody -||- stmtBody -||- ctrlBody

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
  c1 <- multiVarDef
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

inlineComment :: VParser Comment
inlineComment = mergeResult $ do
  string "//"
  t <- manyL $ sat (/= '\n')
  char '\n'
  return $ InlineComment t

comment :: VParser Comment
comment = inlineComment
