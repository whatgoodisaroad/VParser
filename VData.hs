{-# LANGUAGE
    ScopedTypeVariables,
    LambdaCase
  #-}

module VData where

import Data.List (intersperse, nub)
import Data.Monoid

--  Values and lists:
--------------------------------------------------------------------------------

data Select = SL Dim | SR Dim
type Selection = [Select]

dimOf :: Select -> Dim
dimOf = \case { SL d -> d; SR d -> d }

class Variational a where
  select1   :: Select -> a -> a
  mkChoice  :: Dim -> a -> a -> a

select :: Variational v => Selection -> v -> v
select = flip $ foldr select1


type Dim = Char

--  A variational value of type 'a' is either an 'a' value or a choice between 
--  variational 'a' values.
data V a 
  = Choice Dim (V a) (V a) 
  | Val a
  deriving (Eq, Show)

instance Functor V where
  fmap = mapV

instance Monad V where
  return = Val
  Val a >>= f = f a
  Choice d l r >>= f = 
    Choice d (select1 (SL d) $ l >>= f) (select1 (SR d) $ r >>= f)

instance Variational (V a) where
  mkChoice = Choice
  select1 _ (Val a) = Val a
  select1 s (Choice d l r) = case (d == dimOf s, s) of
    (False, _) -> Choice d (select1 s l) (select1 s r)
    (_, SL _) -> select1 s l
    (_, SR _) -> select1 s r

-- A variational list is a list of variational list segments:
type VList' a = [Segment a]

newtype VList a = VL { segments :: [Segment a] }
  deriving (Eq)

--  A variational list segment is either a list of 'a' values or a choice 
--  between variational lists.
data Segment a 
  = SegChoice Char (VList a) (VList a) 
  | Elems [a] 
  deriving (Show, Eq)

type VString = VList Char

instance Show a => Show (VList a) where
  show (VL []) = "[]"
  show (VL vl) = "[" ++ show' vl ++ "]"
    where
      show' :: [Segment a] -> String
      show' = concat . intersperse ", " . concatMap f

      f :: Segment a -> [String]
      f (Elems as) = map show as
      f (SegChoice d l r) = [ d : concat [ "<", show l, " | ", show r, ">" ]]

instance Monoid (VList a) where
  mappend l r = case (normalize l, normalize r) of
    (VL [], r') -> r'
    (l', VL []) -> l'
    (VL l', VL r') -> VL $ normalize' $ l' ++ r'
  mempty = VL []

instance Variational (VList a) where
  select1 = select1VL
  mkChoice d l r = VL [SegChoice d l r]

mkChoiceVL :: Eq a => Dim -> VList a -> VList a -> VList a
mkChoiceVL d l r = prefix +++ (VL [SegChoice d lsuffix rsuffix])
    where
      (prefix, lsuffix, rsuffix) = sharePrefixVL l r

instance Functor VList where
  fmap = mapVL

instance Monad VList where
  return = single
  (>>=) = flip concatMapVL

select1VL :: Select -> VList a -> VList a
select1VL s = VL . concatMap f . segments
  where
    d = dimOf s

    f :: Segment a -> [Segment a]
    f (Elems e) = [Elems e]
    f (SegChoice d' l r) = case (d == d', s) of
      (False, _) -> [SegChoice d' (select1 s l) (select1 s r)]
      (_, SL _) -> segments $ select1 s l
      (_, SR _) -> segments $ select1 s r

--  Does what it says in the type: When V wraps a variational type, we can
--  automatically transfer the choices into the inner type, effectively
--  dropping the V type wrapper.
transfer :: Variational v => V v -> v
transfer (Val a) = a
transfer (Choice d l r) = mkChoice d l' r'
  where
    l' = select1 (SL d) $ transfer l
    r' = select1 (SR d) $ transfer r

liftV :: [a] -> VList a
liftV as = VL [Elems as]

single :: a -> VList a
single a = liftV [a]

(+++) :: VList a -> VList a -> VList a
(+++) = mappend

nilV :: VList a
nilV = mempty

normalize :: VList a -> VList a
normalize = VL . normalize' . segments

normalize' :: VList' a -> VList' a
normalize' [] = []

normalize' ((Elems []):z) = normalize' z

--  Consecutive element lists can be merged
normalize' ((Elems x):(Elems y):z) = normalize' $ (Elems $ x ++ y) : z

--  Consecutive choices in the same dimension can be merged
normalize' ((SegChoice d1 l1 r1):(SegChoice d2 l2 r2):z) = if d1 == d2
  then normalize' $ (SegChoice d1 l12 r12) : z
  else (SegChoice d1 l1' r1') : (normalize' $ (SegChoice d2 l2' r2') : z)
    where
      l1' = select1 (SL d1) l1
      l2' = select1 (SL d2) l2
      r1' = select1 (SL d1) r1
      r2' = select1 (SL d2) r2
      l12 = select1 (SL d1) $ l1 +++ l2
      r12 = select1 (SL d1) $ r1 +++ r2

--  Recurse down choices and remove empty ones
normalize' ((SegChoice d l r):z) = 
  let 
    (l', r') = (normalize l, normalize r) 
  in
  if (null $ segments l') && (null $ segments r')
    then normalize' z
    else (SegChoice d l' r') : normalize' z

normalize' (x:y) = x : normalize' y

mapV :: (a -> b) -> V a -> V b
mapV f (Val a) = Val $ f a
mapV f (Choice d l r) = Choice d (mapV f l) (mapV f r)

--  VList version of map
mapVL :: (a -> b) -> VList a -> VList b
mapVL f = VL . mapVL' f . segments

mapVL' :: (a -> b) -> VList' a -> VList' b
mapVL' f v = map g v
  where
    g (Elems as) = Elems $ map f as
    g (SegChoice d l r) = SegChoice d (mapVL f l) (mapVL f r)

--  VList version of concat
concatVL :: VList (VList a) -> VList a
concatVL = VL . concatMap f . segments
  where
    f :: Segment (VList a) -> [Segment a]
    f (Elems ls) = concatMap segments ls
    f (SegChoice d l r) = let
        l' = concatVL $ select1 (SL d) l
        r' = concatVL $ select1 (SR d) r
      in 
        [SegChoice d l' r']

--  VList version of concatMap
concatMapVL :: (a -> VList b) -> VList a -> VList b
concatMapVL f = concatVL . mapVL f

--  Deconstruct the first element of a VList. The first segment of the list may 
--  be a choice, so the result must be a variational pair. If the list may be 
--  empty under some selection, so the head of the list must be in a Maybe.
unconsVL :: VList a -> V (Maybe a, VList a)
unconsVL (VL []) = Val (Nothing, nilV)
unconsVL (VL ((Elems []):xs)) = unconsVL $ VL xs
unconsVL (VL ((Elems (a:[])):xs)) = Val (Just a, VL xs)
unconsVL (VL ((Elems (a:as)):xs)) = 
  Val (Just a, VL $ normalize' $ (Elems as):xs)
unconsVL (VL ((SegChoice d l r):xs)) = Choice d l' r'
  where
    l' = unconsVL $ l +++ (VL xs)
    r' = unconsVL $ r +++ (VL xs)

consVL :: a -> VList a -> VList a
consVL x (VL xs) = VL $ normalize' $ (Elems [x]) : xs

headVL :: VList a -> V (Maybe a)
headVL = mapV fst . unconsVL

tailVL :: VList a -> V (VList a)
tailVL = mapV snd . unconsVL

--  Take a variational value, and represent it as a variational list of length 1 
--  under any selection.
asSingleton :: V a -> VList a
asSingleton (Val a) = VL [Elems [a]]
asSingleton (Choice d l r) = VL [SegChoice d (asSingleton l) (asSingleton r)]

foldrVL :: (Variational b) => (a -> b -> b) -> b -> VList a -> b
foldrVL _ z (VL []) = z
foldrVL f z (VL (x:xs)) = let z' = foldrVL f z (VL xs) in case x of
  Elems ys        -> foldr f z' ys
  SegChoice d l r -> mkChoice d (foldrVL f z' l) (foldrVL f z' r)

foldr1VL :: (Variational v) => (v -> v -> v) -> VList v -> v
foldr1VL f vs = transfer $ do
  muc <- unconsVL vs
  case muc of
    (Nothing, _) -> error "folr1VL passed empty list"
    (Just x, vs') -> do
      ult <- unconsVL vs'
      case ult of
        (Nothing, _)  -> return x
        (Just y, vs'') -> return $ f x $ foldr1VL f $ y `consVL` vs''

foldlVL :: Variational b => (b -> a -> b) -> b -> VList a -> b
foldlVL f z0 xs0 = g z0 xs0
   where
      g z v = h $ unconsVL v
        where
          h (Val (Nothing, _)) = z
          h (Val (Just x, xs)) = g (f z x) xs
          h (Choice d l r) = mkChoice d (h l) (h r)

foldl1VL :: Variational v => (v -> v -> v) -> VList v -> v
foldl1VL f vl = transfer $ flip fmap (unconsVL vl) $ \case
  (Just x, xs) -> foldlVL f x xs
  (Nothing, _) -> error "foldl1VL passed empty list"

dropVL :: VList a -> V [a]
dropVL = foldrVL (\a b -> fmap (a :) b) (return [])

lengthVL :: VList a -> V Int
lengthVL = foldrVL (const $ mapV (+1)) (Val 0)

allValues :: Eq a => VList a -> [a]
allValues = nub . concatMap f . segments
  where
    f :: Eq a => Segment a -> [a]
    f seg = case seg of
      Elems as -> as
      SegChoice _ l r -> allValues l ++ allValues r

sharePrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
sharePrefix l1 l2 = let 
    prefix = map fst $ takeWhile (uncurry (==)) $ zip l1 l2
    f = drop $ length prefix
  in
  (prefix, f l1, f l2)

sharePrefixVL :: Eq a => VList a -> VList a -> (VList a, VList a, VList a)
sharePrefixVL v1 v2 = let
    (prefix, suff1, suff2) = sharePrefix (segments v1) (segments v2)
  in
  case (suff1, suff2) of
    ((Elems e1):t1, (Elems e2):t2) -> let
        (eprefix, esuff1, esuff2) = sharePrefix e1 e2

        prefix' = VL $ prefix ++ [ Elems eprefix ]
        suff1' = VL $ (Elems esuff1) : t1
        suff2' = VL $ (Elems esuff2) : t2
      in
      (prefix', suff1', suff2')

    -- TODO: (SegChoice d1 l1 r1, SegChoice d2 l2 r2) -> 

    _ -> (VL prefix, VL suff1, VL suff2)

concatMapWhere :: (a -> VList a) -> (a -> Bool) -> VList a -> VList a
concatMapWhere f t = concatMapVL $ \e -> if t e then f e else return e

--  Manifest splits a vlist into elements which are certain (i.e. are at the 
--  top-level, are not children of a choice) and those which are uncertain (i.e. 
--  are the children of at least one choice).
--
--  This is useful when we want to merge elements.
manifest :: VList a -> ([a], VList a)
manifest (VL []) =  ([], nilV)
manifest (VL ((Elems ys):xs)) = let 
    (c, u) = manifest $ VL xs 
  in
  (ys ++ c, u)
manifest (VL (x@(SegChoice _ _ _):xs)) = let
    (c, u) = manifest $ VL xs
  in 
  (c, (VL [x]) +++ u)

--  This function ensures that every branch of a list contains at least one 
--  value (i.e. if a branch were empty, it would have nothing, whereas every 
--  other value is wrapped in Just. In this way, one can map across a VList 
--  while doing something special with empty branches (and ignoring the Just's).
--
--  Consider, for example, when a parser is applied to variational text, and 
--  succeeds in one branch, but fails in another. If this parser is used to 
--  create a variational list, the failure condition signifies the end of the 
--  list (under that selection) and not a failure to parse an entire list.
comprehensive :: VList a -> VList (Maybe a)
comprehensive (VL []) = single Nothing
comprehensive (VL segs) = VL $ flip map segs $ \case
  Elems []        -> Elems [Nothing]
  Elems xs        -> Elems $ map Just xs
  SegChoice d l r -> SegChoice d (comprehensive l) (comprehensive r)
