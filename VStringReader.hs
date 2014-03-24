module VStringReader (readVString) where

  import VData
  import Text.ParserCombinators.Parsec

  plain :: Parser (Segment Char)
  plain = (many1 $ noneOf "#") >>= return . Elems

  segchoice :: Parser (Segment Char)
  segchoice = do
    char '#'
    dim <- oneOf $ ['A' .. 'Z']
    char '<'
    lalt <- vlist
    string "#,"
    ralt <- vlist
    string "#>"
    return $ SegChoice dim lalt ralt

  vlist :: Parser VString
  vlist = (many $ (try segchoice) <|> plain) >>= return . VL

  readVString :: String -> VString
  readVString s = case parse vlist "" s of { Right v -> v }
