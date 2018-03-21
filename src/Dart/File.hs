{-# LANGUAGE OverloadedStrings #-}

module Dart.File where

import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           System.Directory
import           System.IO

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

specToFile :: FilePath -> Spec ann -> IO ()
specToFile rootDir spec =
  let path = pathForSpec rootDir spec
      file = makePath path <> ".dart"
      body = vsep $ declarations spec
  in do
    ensureDirectory rootDir spec
    withFile (unpack file) WriteMode (`hPutDoc`  body)
