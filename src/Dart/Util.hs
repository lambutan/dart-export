module Dart.Util where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

{-|
A service function used to indent a list of prettified lines by a specified
number of spaces
-}
indentStack :: Int -> [Doc ann] -> Doc ann
indentStack n lst = indent n (align $ vsep lst)

{-|
A service function to enclose a prettified text into braces and insert newlines
-}
hBraces :: Doc ann -> Doc ann
hBraces doc = braces (hardline <> doc <> hardline)
