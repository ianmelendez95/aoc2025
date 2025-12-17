module Z3 where 

import Turtle
import Data.Text qualified as T

callZ3 :: T.Text -> IO T.Text
callZ3 = call "z3" ["-in"]
