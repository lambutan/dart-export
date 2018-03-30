{-# LANGUAGE OverloadedStrings #-}

module Dart.File
        ( specToFile
        , Spec(..)
        ) where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Directory
import           System.IO

{-|
A data structure that contains the Namespace for the output file and a list
of prettified Dart class definitions obtained by converting Haskell types.
-}
data Spec ann = Spec{
                namespace    :: [ Text ]
              , declarations :: [ Doc ann ]
                }

makePath :: [Text] -> Text
makePath = intercalate "/"

pathForSpec :: FilePath -> Spec ann -> [Text]
pathForSpec rootDir spec = pack rootDir : namespace spec

ensureDirectory :: FilePath -> Spec ann -> IO ()
ensureDirectory rootDir spec =
  let dir = makePath . Prelude.init $ pathForSpec rootDir spec
  in createDirectoryIfMissing True (unpack dir)

{-|
Write the declarations given in "Spec" to the path provided
-}
specToFile :: FilePath -> Spec ann -> IO ()
specToFile rootDir spec =
  let path = pathForSpec rootDir spec
      file = makePath path <> ".dart"
      body = vsep $ declarations spec
  in do
    ensureDirectory rootDir spec
    withFile (unpack file) WriteMode (`hPutDoc`  body)
