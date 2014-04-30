import VData
import VParser

data Name
  = Name Char
  | NChc Dim Name Name
  deriving Eq

data VLC
  = Var Char
  | App VLC VLC
  | Abs Name VLC
  | EChc Dim VLC VLC
  deriving Eq

instance Variational Name where
  mkChoice = NChc
  select1 s (NChc d l r) = case (dimOf s == d, s) of
    (False, _) -> NChc d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 _ n = n

instance Variational VLC where
  mkChoice = EChc
  select1 s (EChc d l r) = case (dimOf s == d, s) of
    (False, _) -> EChc d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r
  select1 s (Abs c e) = Abs c $ select1 s e
  select1 _ v = v

instance Show Name where
  show (Name c) = [c]
  show (NChc d l r) = concat [ [d], "<", show l, ",", show r, ">" ]

instance Show VLC where
  show (Var c) = [c]
  show (App e1 e2) = concat [ show e1, show e2 ]
  show (Abs x e) = concat [ "(λ", show x, ".", show e, ")" ]
  show (EChc d l r) = concat [ [d], "<", show l, ",", show r, ">" ]

namep = mergeResult $ fmap Name $ oneOf ['a'..'z']

vlcp = expp

expp = mergeResult $ do 
  exps <- manyV $ parenp -||- absp -||- valp
  return $ foldr1VL App exps

parenp = mergeResult $ do
  char '('
  e <- expp
  char ')'
  return e

valp = mergeResult $ fmap Var $ oneOf ['a'..'z']

absp = mergeResult $ do
  oneOf "\\λ"
  x <- namep
  char '.'
  e <- expp
  return $ Abs x e
