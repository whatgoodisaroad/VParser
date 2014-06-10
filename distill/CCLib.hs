{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module CCLib where

  import Data.List

  import Text.ParserCombinators.Parsec
  import Text.ParserCombinators.Parsec.Char
  import qualified Data.Algorithm.Diff as D

--  Types
--------------------------------------------------------------------------------

  data V d a
    = Plain [a]
    | Choice d [V d a] [V d a]
    deriving (Eq)

  type VText = [V String Char]

  instance Show (V String Char) where
    show (Plain x) = x
    show (Choice d l r) = concat [
        escape,
        d,
        "<",
        concat $ map show l,
        escape, ",",
        concat $ map show r,
        escape, ">"
      ]

  ppVText :: VText -> String
  ppVText = concatMap show

  escape = "@"

  --instance (Show d, Show a) => Show (V d a) where
  --  show (Plain x) = show x
  --  show (Choice d l r) = concat [
  --      "#",
  --      show d,
  --      "<",
  --      concat $ map show l,
  --      "#,",
  --      concat $ map show r,
  --      ">"
  --    ]

  data Sel d = LSel d | RSel d
  type Selection d = [Sel d]

  data Alt = L | R deriving Eq

  instance Show (Sel String) where
    show (LSel d) = d ++ ".l"
    show (RSel d) = d ++ ".r"

  -- Use a sequence of child indices as a path type:
  type Path = [Int]
  type VMap = [(Int, Path)]

  lift :: [a] -> [V d a]
  lift = (:[]) . Plain


--  Example Values
--------------------------------------------------------------------------------

  -- Example #1: abc#A<x, #B<y, z>>#C<w, v>def
  example1 :: VText
  example1 = [
      Plain "abc",
      Choice "A"
        [Plain "x"] 
        [Choice "B" [Plain "y"] [Plain "z"]],
      Choice "C"
        [Plain "w"]
        [Plain "v"],
      Plain "def"
    ]

  exampleSel :: Selection String
  exampleSel = [ RSel "A", LSel "B", RSel "C" ]


--  Parsers
--------------------------------------------------------------------------------

  ccFile :: GenParser Char st VText
  ccFile = do
    doc <- many ccConstruct
    eof
    return doc

  ccConstruct :: GenParser Char st (V String Char)
  ccConstruct = do
    try ccChoice <|> ccPlain

  ccPlain :: GenParser Char st (V String Char)
  ccPlain = fmap Plain $ many1 $ noneOf escape

  ccChoice :: GenParser Char st (V String Char)
  ccChoice = try $ do
    string escape
    dim <- many1 alphaNum
    char '<'
    left <- many ccConstruct
    string escape
    char ','
    right <- many ccConstruct
    string escape
    char '>'
    return $ Choice dim left right

  ccParser :: String -> Either ParseError VText
  ccParser = parse ccFile ""

  selector :: GenParser Char st (Sel String)
  selector = do
    d <- many letter
    char '.'
    a <- oneOf "lrLR"
    return $ if a `elem` "lL"
      then LSel d
      else RSel d

  selection :: GenParser Char st [Sel String]
  selection = selector `sepBy` (oneOf ", ")

  selParser :: [Char] -> Either ParseError [Sel String]
  selParser = parse selection ""


--  Selection Semantics
--------------------------------------------------------------------------------

  isSelectedIn :: Eq d => d -> [Sel d] -> Bool
  isSelectedIn d = elem d . map f
    where
      f (LSel d) = d
      f (RSel d) = d

  asSelectedIn :: Eq d => d -> [Sel d] -> Alt
  asSelectedIn d ((LSel d'):ss) = if d == d' then L else d `asSelectedIn` ss
  asSelectedIn d ((RSel d'):ss) = if d == d' then R else d `asSelectedIn` ss

  validateSelection :: Either ParseError [Sel String] -> Either String [Sel String]
  validateSelection (Left e) = Left $ show "Selectors failed to parse"
  validateSelection (Right ss) = if nodup then Right ss else Left dupmsg
    where
      f [] = Nothing
      f (c:cs) = if c `elem` cs then Just c else f cs
      dup = f $ map dof ss
      dof (LSel d) = d
      dof (RSel d) = d
      nodup = dup == Nothing
      dupmsg = "Duplicate selection for dimension " ++ dim
        where (Just dim) = dup

  applySelector :: Eq d => Sel d -> V d a -> [V d a]
  applySelector _ (Plain a) = [Plain a]
  applySelector s@(LSel d) (Choice d' l r) = if d == d'
    then concatMap (applySelector s) l
    else [Choice d' (concatMap (applySelector s) l) (concatMap (applySelector s) r)]
  applySelector s@(RSel d) (Choice d' l r) = if d == d'
    then concatMap (applySelector s) r
    else [Choice d' (concatMap (applySelector s) l) (concatMap (applySelector s) r)]

  applySelectionWithMap ::
    forall d a. Eq d => Selection d -> [V d a] -> ([a], VMap)
  applySelectionWithMap s = a [0] 0
    where
      a :: Path -> Int -> [V d a] -> ([a], VMap)
      a _ _ [] = ([], [])
      a p o ((Plain b):vs) = 
        (b, (o, reverse p)) ^: a (psucc p) (o + length b) vs
      a p o ((Choice d l r):vs) =
        a' ^++ a (psucc p) (o + length v') vs
        where
          c = case d `asSelectedIn` s of { L -> l; R -> r }
          a'@(v', _) = a (0:p) o c

      (^:) :: ([a], (Int, Path)) -> ([a], VMap) -> ([a], VMap)
      (v, p) ^: (vs, ps) = (v ++ vs, p:ps)

      (^++) :: ([a], VMap) -> ([a], VMap) -> ([a], VMap)
      (v, p) ^++ (vs, ps) = (v ++ vs, p ++ ps)

      psucc (p:ps) = succ p : ps

  applySelection :: Eq d => [Sel d] -> [V d a] -> [a]
  applySelection s v = fst $ applySelectionWithMap s v

  (-!-) :: Eq d => [Sel d] -> [V d a] -> [a]
  (-!-) = applySelection

  mapToF :: VMap -> Int -> Path
  mapToF ((i, p):ms) j = if i <= j then p else mapToF ms j

  normalize :: Eq d => [V d a] -> [V d a]
  normalize [] = []
  normalize ((Plain x):(Plain y):z) = normalize $ Plain (x ++ y) : z
  normalize (x@(Plain _):y) = x : normalize y
  normalize ((Choice d l r):(Choice d' l' r'):z) = if d == d'
    then  normalize $ Choice d (normalize $ l ++ l') (normalize $ r ++ r') : z
    else  (Choice d (normalize l) (normalize r)) :
            (Choice d' (normalize l') (normalize r')) :
              normalize z
  normalize ((Choice d l r):z) = 
    Choice d (normalize l) (normalize r) : normalize z

  denormalizeV :: forall d a. Eq d => Selection d -> [V d a] -> [Int] -> [V d a]
  denormalizeV s v b = fst $ d s v b
    where 
      (^:) :: V d a -> ([V d a], [Int]) -> ([V d a], [Int])
      x ^: (y, z) = (x:y, z)

      d :: Selection d -> [V d a] -> [Int] -> ([V d a], [Int])
      d _ [] bs = ([], bs)
      d _ vs [] = (vs, [])
      d s vs (0:bs) = d s vs bs
      d s ((Choice dim l r):vs) bs = case dim `asSelectedIn` s of
        L -> e id   l r
        R -> e flip r l
        where
          e f x y = ((f $ Choice dim) x' y) ^: (d s vs bs')
            where
              (x', bs') = d s x bs
      d s ((Plain x):vs) (b:bs) = case length x `compare` b of
        EQ -> (Plain x) ^: (d s vs $ tail bs')
        LT -> (Plain x) ^: (d s vs bs'')
        GT -> (Plain x') ^: (d s ((Plain x''):vs) $ tail bs')
        where
          bs'   = map (subtract b) $ b:bs
          bs''  = map (subtract $ length x) (b:bs)
          x'    = take b x
          x''   = drop b x

  dimensions :: Eq d => [V d a] -> [d]
  dimensions [] = []
  dimensions ((Plain _):vs) = dimensions vs
  dimensions ((Choice d l r):vs) = 
    [d] `union` 
      (dimensions l) `union` 
      (dimensions r) `union`
      (dimensions vs)

  nextDimension :: (Eq d, Ord d, Enum d) => [V d a] -> d
  nextDimension = succ . maximum . dimensions

  latest :: Eq d => [V d a] -> [Sel d]
  latest = map RSel . dimensions

--  Customized diff
--------------------------------------------------------------------------------
  
  data Diff a = Same [a] | Different [a] [a] deriving Show

  collectDiff :: [D.Diff [a]] -> [Diff a]
  collectDiff [] = []
  collectDiff ((D.Both x _):ds) = Same x : collectDiff ds
  collectDiff ((D.First x):(D.Second y):ds) = Different x y : collectDiff ds
  collectDiff ((D.First x):ds) = Different x [] : collectDiff ds
  collectDiff ((D.Second x):ds) = Different [] x : collectDiff ds

  (-?-) :: Eq a => [a] -> [a] -> [Diff a]
  old -?- new = collectDiff $ D.getGroupedDiff old new

  _diffAsCC :: [Diff Char] -> String
  _diffAsCC [] = ""
  _diffAsCC ((Same x):ds) = x ++ _diffAsCC ds
  _diffAsCC ((Different x y):ds) = concat [ 
      escape, 
      "<", 
      x, 
      escape, 
      ",", 
      y, 
      escape, 
      ">", 
      _diffAsCC ds 
    ]

  partitionDiff :: [Diff a] -> [Int] -> [Diff a]
  partitionDiff d bs = p d bs
    where 
      p :: [Diff a] -> [Int] -> [Diff a]
      p [] _ = []
      p ds [] = ds
      p ds (0:bs) = p ds bs
      p (d:ds) (b:bs) = case d of
        Same x        -> t x x $ flip $ const Same
        Different x y -> t x y Different
        where 
          t x y f = case length x `compare` b of
            EQ -> f x y   : (p ds $ tail bs')
            LT -> f x y   : (p ds bs'')
            GT -> f x' y' : (p (f x'' y'' : ds) $ tail bs')
            where
              bs' = map (subtract b) $ b:bs
              bs'' = map (subtract $ length x) $ b:bs
              x'  = take b x
              x'' = drop b x
              y'  = take b y
              y'' = drop b y

  diffBoundaries :: [Diff a] -> [Int]
  diffBoundaries [] = []
  diffBoundaries ((Same x):ds) = 0 : (map (+ length x) $ diffBoundaries ds)
  diffBoundaries ((Different x _):ds) = 0 : (map (+ length x) $ diffBoundaries ds)

  diffL :: [Diff a] -> [a]
  diffL [] = []
  diffL ((Same x):ds) = x ++ diffL ds
  diffL ((Different x _):ds) = x ++ diffL ds

  diffR :: [Diff a] -> [a]
  diffR [] = []
  diffR ((Same x):ds) = x ++ diffR ds
  diffR ((Different _ x):ds) = x ++ diffR ds

--  Distillation
--------------------------------------------------------------------------------

  distill :: 
    forall d a. 
    (Eq d, Eq a, Show a, Show d) => 
    d -> [V d a] -> Selection d -> [a] -> [V d a]
  distill dim v s n = normalize $ fst $ l pv d
    where
      (o, m) = s `applySelectionWithMap` v
      d = partitionDiff (o -?- n) $ map fst m
      pv = denormalizeV s v $ diffBoundaries d

      (^:) :: V d a -> ([V d a], [Diff a]) -> ([V d a], [Diff a])
      x ^: (y, z) = (x:y, z)

      l :: [V d a] -> [Diff a] -> ([V d a], [Diff a])
      
      l [] ((Different [] x):ds) = ([Choice dim [] [Plain x]], ds)

      l [] ds = ([], ds)

      -- Unchanged plain text:
      l ((Plain x):vs) ((Same _):ds) = Plain x ^: (l vs ds)

      -- Addition:
      l vs ((Different [] x):ds) = Choice dim [] [Plain x] ^: (l vs ds)

      -- Removal:
      l ((Plain x):vs) ((Different x' []):ds) = 
        Choice dim [Plain x] [] ^: (l vs ds)

      -- Changed plain:
      l ((Plain x):vs) ((Different x' y):ds) = if x == x'
        then Choice dim [Plain x] [Plain y] ^: (l vs ds)
        else error $ concat [ "Mismatch ", show x, " /= ", show x' ]

      -- Recurse downward along choice:
      l ((Choice dim' left right):vs) ds = case dim' `asSelectedIn` s of
        L -> l' id    left right
        R -> l' flip  right left
        where
          l' f x y = ((f $ Choice dim') x' y) ^: (l vs ds')
            where
              (x', ds') = l x ds
