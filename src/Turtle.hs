module Turtle (call) where 

import Turtle.Prelude
import Turtle.Line
import Turtle.Shell
import Data.Text qualified as T

call :: T.Text -> [T.Text] -> T.Text -> IO T.Text
call cmd args stdin = do
  (exit_code, stdout, stderr) <- procStrictWithErr cmd args $ select (textToLines stdin)
  undefined
  -- if exit_code /= 0
  -- then error $ "Exit with code " ++ show exit_code ++ " and stdout: " ++ show stderr
  -- else pure stdout

execZ3 :: T.Text -> IO T.Text
execZ3 z3_script = do 
  let shell_lines = select . textToLines $ z3_script
  strict $ inproc "z3" ["-in"] shell_lines
