{-# LANGUAGE
    ScopedTypeVariables,
    LambdaCase,
    GADTs
  #-}

module VParser where

import Data.List (nub, partition)

import VData

--  Parser:
--------------------------------------------------------------------------------

--  Parser type
data VParser a where
  P   :: (VString -> VList (a, VString)) -> VParser a
  PV  :: (Eq a, Variational a) => (VString -> VList (a, VString)) -> VParser a

runParser :: VParser a -> VString -> Either String a
runParser p v = case fst $ manifest $ papply p v of
  [] -> Left "Parser failed to find anything"
  ((a, r):_) -> if (null $ allValues r)
    then Right a
    else Left $ "Failed to consume whole input: " ++ show r

getFn :: VParser a -> VString -> VList (a, VString)
getFn (P f) = f
getFn (PV f) = merge . f

normalizeContext :: forall a. VList (a, VString) -> VList (a, VString)
normalizeContext = VL . map f . segments
  where
    f :: Segment (a, VString) -> Segment (a, VString)
    f (SegChoice d l r) = SegChoice d (select1Snd (SL d) l) (select1Snd (SR d) r)
    f s = s

    select1Snd :: Select -> VList (a, VString) -> VList (a, VString)
    select1Snd s = fmap (\(a, rem) -> (a, select1 s rem)) . normalizeContext

--  Monadic methods: inject and bind
inject :: a -> VParser a
inject a = P $ \s -> single (a, s)

bind :: forall a b. VParser a -> (a -> VParser b) -> VParser b
bind p g = P $ normalize . normalizeContext . f'
  where
    f = getFn p

    f' s = gs
      where
        fs :: VList (a, VString)
        fs = f s

        gss :: VList (VList (b, VString))
        gss = mapVL (\(a, s') -> let (P h) = g a in h s') fs

        gs :: VList (b, VString)
        gs = normalize $ concatVL gss

instance Monad VParser where
  return = inject
  (>>=) = bind

instance Functor VParser where
  fmap f p = let g = getFn p in P $ fmap (\(a, s) -> (f a, s)) . g

normalizeRemainders :: VList (a, VString) -> VList (a, VString)
normalizeRemainders = mapVL $ \(a, s) -> (a, normalize s)

--  Lifted application for VParser
papply :: VParser a -> VString -> VList (a, VString)
papply p s = let f = getFn p in normalize $ normalizeContext $ f s

--  Parse the next character from input
item :: VParser Char
item = P $ \s -> concatMapVL f $ asSingleton $ unconsVL s
  where
    f (Just h, t) = single (h, t)
    f (Nothing, _) = nilV

zero :: VParser a
zero = P $ const nilV

sat :: (Char -> Bool) -> VParser Char
sat p = do 
  c <- item
  if p c 
    then return c 
    else zero

char :: Char -> VParser Char
char c = sat (== c)

string :: String -> VParser String
string "" = return ""
string (c:cs) = do { char c; string cs; return (c:cs) }

alt :: VParser a -> VParser a -> VParser a
alt (P p)   (P q)   = P   $ \s -> p s +++ q s
alt (PV p)  (P q)   = PV  $ \s -> p s +++ q s
alt (P p)   (PV q)  = PV  $ \s -> p s +++ q s
alt (PV p)  (PV q)  = PV  $ \s -> p s +++ q s

first :: VParser a -> VParser a
first p = let 
    f = getFn p 
  in 
    P $ \s -> concatVL $ asSingleton $ mapV g $ headV $ f s
  where
    g (Just h) = single h
    g Nothing = nilV

(-|-) :: VParser a -> VParser a -> VParser a
p -|- q = first $ p `alt` q

(-||-) :: Variational a => VParser a -> VParser a -> VParser a
p -||- q = first $ mergeContext $ p `alt` q

manyL1 :: VParser a -> VParser [a]
manyL1 p = do
  x <- p
  xs <- manyL p
  return $ x:xs

manyL :: VParser a -> VParser [a]
manyL p = manyL1 p -|- return []

manyV :: Eq a => VParser a -> VParser (VList a)
manyV p = P $ \s -> applyUntilStable (mergeVL . advance p) $ single (nilV, s)

--  Given a parser of a, and a list of parser contexts for vlists of a, advance
--  each element with the parser and append the result to the list. If the 
--  parser fails, consider this the end of the list and leave the conetxt 
--  unaffected.
advance ::
  forall a.
  Eq a => 
  VParser a -> VList (VList a, VString) -> VList (VList a, VString)
advance p = normalizeContext . VL . concatMap g . segments
  where
    f = getFn p

    g :: Segment (VList a, VString) -> VList' (VList a, VString)
    g (Elems cs) = normalize' $ concatMap advanceContext cs
    g (SegChoice d l r) = [SegChoice d (advance p l) (advance p r)]

    --  Given a single parser context, apply the parser to move it forward, but
    --  if it fails, do nothing to the context.
    advanceContext :: (VList a, VString) -> VList' (VList a, VString)
    advanceContext (vl, s') = segments $ mapVL append cs'
      where 
        --  Apply the parser and make it "conprehensive" so that failures can be
        --  mapped.
        cs' = comprehensive $ normalize $ f s'

        --  Take a single parser result and append it to the context or drop it
        --  as appropriate.
        append :: Maybe (a, VString) -> (VList a, VString)
        
        --  Failure to advance --> end of vlist
        append Nothing = (vl, s')

        --  Successful advance --> append to vlist
        append (Just (a, s'')) = (normalize vl +++ single a, s'')

merge :: (Variational a, Eq a) => VList (a, VString) -> VList (a, VString)
merge = mergeBy mkChoice

mergeVL :: (Eq a) => VList (VList a, VString) -> VList (VList a, VString)
mergeVL = mergeBy mkChoiceVL

--  Given a vlist of parser contexts of vlists, merge what can be merged. This 
--  is done bottom-up. This is tricky because it's a structre-sensitive search 
--  problem combined with a list mutation, 
mergeBy :: 
  forall a. 
  (Variational a, Eq a) => 
  (Dim -> a -> a -> a) -> VList (a, VString) -> VList (a, VString)
mergeBy m = normalizeContext . VL . concatMap g . segments
  where
    g :: Eq a => Segment (a, VString) -> VList' (a, VString)
    g (Elems cs) = [Elems cs]
    g (SegChoice d l r) = [Elems $ map (mergePair d) toMerge, SegChoice d l' r']
      where
        --  Recrse on the left and right children (bottom up), and partition 
        --  them into certain and uncertain lists.
        (cl, ul) = manifest $ merge l
        (cr, ur) = manifest $ merge r

        --  Get the complete list of all possible remainders from either the 
        --  left or right certian lists. These are the remainders which might 
        --  be shared between merge candidates.
        allRemainders :: [VString]
        allRemainders = nub $ map snd $ cl ++ cr

        --  Fold over each remainder, to find all the mergible elements.
        (toMerge, cl', cr') = foldr partitionByRem ([], cl, cr) allRemainders

        --  Construct the complete lists of unmerged elements.
        l' = liftV cl' +++ ul
        r' = liftV cr' +++ ur

    mergePair :: Dim -> ((a, a), VString) -> (a, VString)
    mergePair d ((l, r), rem) = (m d l r, rem)

--  Take a remainder, a list of mergible contexts, and two lists of 
--  contexts yet to be searched for merges, and produce a new list of 
--  mergible contexts, as well as the search lists sans those which 
--  contributed new values to the merge list.
partitionByRem :: 
  VString -> 
  ([((b, b), VString)], [(b, VString)], [(b, VString)]) -> 
  ([((b, b), VString)], [(b, VString)], [(b, VString)])
partitionByRem rem (cs, ls, rs) = 
  let
    --  Partition by what does and does not have the given remainder:
    part = partition ((== rem) . snd) 
    (cl, ls') = part ls
    (cr, rs') = part rs

    cs' = [ ((al, ar), rem) | al <- map fst cl, ar <- map fst cr ]
  in
  if null cl || null cr

    --  Either there were no contexts on the left or right with the given
    --  remainder, thus, nothing can be merged. Change nothing.
    then (cs, ls, rs)

    --  There were mergeable elements, append them to the merge list, and 
    --  yeield the search lists sans the mergables.
    else (cs ++ cs', ls', rs')

applyUntilStable :: Eq a => (a -> a) -> a -> a
applyUntilStable f a = let a' = f a in if a' == a 
  then a 
  else applyUntilStable f a'

--  This function ensures that every branch of a list contains at least one 
--  value (i.e. if a branch were empty, it would have nothing, whereas every 
--  other value is wrapped in Just. In this way, one can map across a VList 
--  while doing something special with empty branches (and ignoring the Just's).
--
--  Consider, for example, when a parser is applied to variational text, and 
--  succeeds in one branch, but fails in another. If this parser is used to 
--  create a variational list, the failure condiftion signifies the end of the 
--  list (under that selection) and not a failure to parse an entire list.
comprehensive :: VList a -> VList (Maybe a)
comprehensive (VL []) = single Nothing
comprehensive (VL segs) = VL $ flip map segs $ \case
  Elems []        -> Elems [Nothing]
  Elems xs        -> Elems $ map Just xs
  SegChoice d l r -> SegChoice d (comprehensive l) (comprehensive r)

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

oneOf :: [Char] -> VParser Char
oneOf = sat . flip elem

sepBy1V :: Eq a => VParser a -> VParser b -> VParser (VList a)
sepBy1V p s = do
  a <- p
  as <- manyV $ s >> p
  return $ a `consVL` as

sepByV :: Eq a => VParser a -> VParser b -> VParser (VList a)
sepByV p s = sepBy1V p s -|- return nilV

mergeContext :: VParser a -> VParser a
mergeContext (PV f) = PV $ merge . f
mergeContext p = p

mergeResult :: (Eq a, Variational a) => VParser a -> VParser a
mergeResult = PV . getFn

