module VStringReader (readVString, vstringFromFile) where

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

  vlistFile :: Parser VString
  vlistFile = do { v <- vlist; eof; return v; }

  readVString :: String -> VString
  readVString s = case parse vlistFile "" s of
    Right v -> v
    Left m -> error $ show m

  vstringFromFile :: FilePath -> IO VString
  vstringFromFile = fmap readVString . readFile
