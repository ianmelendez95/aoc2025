module Parse (
  Parser,
  parse,
  symbol,
  lexeme,
  hsymbol,
  hlexeme,
  eof,
  between,
  C.char,
  satisfy,
  some,
  sepBy1,
  L.decimal,
  takeWhileP,
  takeWhile1P,
  try,
  many,
  liftA2,
  C.eol,
  (<|>),
  sepBy
  ) where 

import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void
import Data.Text qualified as T

type Parser = Parsec Void T.Text

parse :: Parser a -> T.Text -> a
parse p txt = 
  case runParser p "test.txt" txt of 
    Right r -> r
    Left e -> error . errorBundlePretty $ e

hsymbol :: T.Text -> Parser T.Text
hsymbol = L.symbol hspace

hlexeme :: Parser a -> Parser a
hlexeme = L.lexeme hspace

hspace :: Parser ()
hspace = L.space C.hspace1 (L.skipLineComment "--") (L.skipBlockComment "{--" "--}")

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{--" "--}")


