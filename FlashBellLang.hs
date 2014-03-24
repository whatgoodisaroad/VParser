import VData
import VParser

--  Command data type
data Cmd 
  = Flash 
  | Bell 
  | Procedure (V String) Prog 
  | Invoke (V String)
  deriving (Show, Eq)

type Prog = VList Cmd

p1 :: Prog
p1 = liftV [Flash, Bell]

p2 :: Prog
p2 = p1 +++ liftV [Procedure (Val "f") $ liftV [Flash, Bell] ]

p3 :: Prog
p3 = p1 +++ VL [SegChoice 'A' (single Bell) (single Flash)] +++ p1

i3 :: VString
i3 = VL [
    Elems "flash;bell;", 
    SegChoice 'A' (liftV "bell;") (liftV "flash;"),
    Elems "flash;bell;"
  ]

i4 :: VString
i4 = VL [
    Elems "flash;", 
    SegChoice 'A' (liftV "bell;") nilV,
    Elems "bell;"
  ]

i5 :: VString
i5 = VL [
    Elems "flash;",
    SegChoice 'A' 
      (liftV "bell;")
      (VL [SegChoice 'B' 
        (liftV "flash;")
        (liftV "flash;bell;")
      ]),
    Elems "flash;"
  ]

flashParser :: VParser Cmd
flashParser = string "flash" >> return Flash

bellParser :: VParser Cmd
bellParser = string "bell" >> return Bell

procNameParser :: VParser (V String)
procNameParser = fmap dropVL $ manyV $ oneOf ['a'..'z']

procParser :: VParser Cmd
procParser = do
  string "proc "
  name <- procNameParser
  string "{"
  body <- progParser
  string "}"
  return $ Procedure name body

invokeParser :: VParser Cmd
invokeParser = string "exec " >> procNameParser >>= return . Invoke

cmdParser :: VParser Cmd
cmdParser = procParser -|- invokeParser -|- flashParser -|- bellParser

progParser :: VParser Prog
progParser = manyV $ do { c <- cmdParser; char ';'; return c }
