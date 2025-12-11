module Parse (
  Parser,
  symbol,
  lexeme,
  between,
  C.char,
  satisfy,
  some,
  sepBy1,
  L.decimal
  ) where 

import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void
import Data.Text qualified as T

type Parser = Parsec Void T.Text

symbol :: T.Text -> Parser T.Text
symbol = L.symbol space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

space :: Parser ()
space = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{--" "--}")


