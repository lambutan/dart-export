module Dart.Util where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

indentStack :: Int -> [Doc ann] -> Doc ann
indentStack n lst = indent n (align $ vsep lst)

hBraces :: Doc ann -> Doc ann
hBraces doc = braces (hardline <> doc <> hardline)
