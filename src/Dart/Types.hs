{-# LANGUAGE OverloadedStrings #-}

module Dart.Types where

import           Dart.Converter
import           Dart.Util
import           Data.Proxy
import           Data.Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           GHC.Generics

{-|
Definitions of Pretty Printer's 'pretty' function for the convertible types
-}

instance Pretty DartPrimitive where
  pretty DartBool     = pretty ("bool" :: Text)
  pretty DartDouble   = pretty ("double" :: Text)
  pretty DartInt      = pretty ("int" :: Text)
  pretty DartString   = pretty ("String" :: Text)
  -- pretty (DartOptional a) = pretty a
  pretty (DartList a) = pretty ("List" :: Text) <> angles ( pretty a )

instance Pretty DartTypeLabel where
  pretty (DartLabel txt)        = pretty txt
  pretty (DartPrimitiveLabel a) = pretty a

instance Pretty DartConstructor where
 pretty (DartConstructor txt _) = pretty txt

instance Pretty DartDataType where
  pretty (DartPrimitive _)            = emptyDoc
  pretty dt@(DartDataType title cons) =
    case Prelude.length cons of
      1 -> vsep $ renderSingleConstructor title <$> cons
      _ -> vsep $ renderDataType dt : punctuate hardline (renderConstructor title <$> cons)

------------------------------------

{-|
Instructions for rendering specific elements
-}

renderDataType :: DartDataType -> Doc ann
renderDataType (DartPrimitive _)         = emptyDoc
renderDataType (DartDataType title cons) = pretty ( "abstract class " <> title )
                                       <+> hBraces
                                       (indentStack 1 $ pretty <$> ["final String _tag;", "", title <> "(this._tag)" ])
                                       <> hardline

renderField :: DartField -> Doc ann
renderField (DartField name contents) = pretty ("final" :: Text) <+> pretty contents <+> pretty name <> semi

renderConstructor :: Text -> DartConstructor -> Doc ann
renderConstructor parent (DartConstructor txt fields) =
                let
                  prettyOwnFields = (\(DartField name _) -> pretty $ "this." <> name) <$> fields
                  fieldTuple = tupled prettyOwnFields
                  dartConstructor = hardline <> pretty txt <> fieldTuple <+> colon <+> pretty ("super" :: Text) <> parens (squotes $ pretty txt) <> semi
                in
                  pretty ("class " <> txt <> " extends " <> parent)
              <+> hBraces (indentStack 1 ((renderField <$> fields) ++ [ dartConstructor ]))

renderSingleConstructor :: Text -> DartConstructor -> Doc ann
renderSingleConstructor parent (DartConstructor txt fields) =
                let
                  prettyOwnFields = (\(DartField name _) -> pretty $ "this." <> name) <$> fields
                  fieldTuple = tupled prettyOwnFields
                  dartConstructor = hardline <> pretty txt <> fieldTuple <> semi
                in
                  pretty ("class " <> parent)
              <+> hBraces (indentStack 1 ((renderField <$> fields) ++ [ dartConstructor ]))

getDartType :: (DartTypeConvertible a) => Proxy a -> Doc ann
getDartType prx = pretty $ toDartType prx
