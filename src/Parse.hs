module Parse (
  Parser
  ) where 

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Void
import Data.Text qualified as T

type Parser a = Parsec Void T.Text a

space :: Parser ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{--" "--}")


