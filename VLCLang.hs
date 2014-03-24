import VData
import VParser

-- VLC Example:

data VLC
  = Var Char
  | App VLC VLC
  | Abs Char VLC
  | Chc Dim VLC VLC
  deriving Eq

instance Variational VLC where
  mkChoice = Chc
  select1 s (Chc d l r) = case (dimOf s == d, s) of
    (False, _) -> Chc d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (Abs c e) = Abs c $ select1 s e
  select1 _ v = v

instance Show VLC where
  show (Var c) = [c]
  show (App e1 e2) = concat [ show e1, show e2 ]
  show (Abs x e) = concat [ "(λ", [x], ".", show e, ")" ]
  show (Chc d l r) = concat [ [d], "<", show l, ",", show r, ">" ]

vlcp = expp

expp = mergeResult $ do 
  exps <- manyV $ parenp -|- absp -|- valp
  
  --  TODO: If the resulting list is empty, we need to fail the parser rather 
  --  than passing it into foldr1VL, which is undefined for that function.

  return $ foldr1VL App exps

parenp = mergeResult $ do
  char '('
  e <- expp
  char ')'
  return e

valp = mergeResult $ fmap Var $ oneOf ['a'..'z']

absp = mergeResult $ do
  oneOf "\\λ"
  x <- oneOf ['a'..'z']
  char '.'
  e <- expp
  return $ Abs x e
