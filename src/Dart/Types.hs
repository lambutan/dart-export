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
  -- pretty (TSOptional a) = pretty a
  pretty (DartList a) = pretty ("List" :: Text) <> angles ( pretty a )

instance Pretty DartTypeLabel where
  pretty (DartLabel txt)        = pretty txt
  pretty (DartPrimitiveLabel a) = pretty a

instance Pretty DartConstructor where
 pretty (DartConstructor txt _) = pretty txt

instance Pretty DartDataType where
  pretty (DartPrimitive _)            = emptyDoc
  pretty dt@(DartDataType title cons) = renderDataType dt <> hardline
  -- pretty dt@(DartDataType title cons) = renderAlias dt <> hardline <> vsep (renderClass <$> cons) <> hardline

------------------------------------

{-|
Instructions for rendering specific elements
-}

renderDataType :: DartDataType -> Doc ann
renderDataType (DartPrimitive _)         = emptyDoc
renderDataType (DartDataType title cons) = pretty( "abstract class " <> title ) <+> hBraces ""


-- renderField :: TSField -> Doc ann
-- renderField (TSField name contents) =
--   case contents of
--     TSPrimitiveLabel(TSOptional _) -> pretty ("readonly " <> name <> "?:") <+> pretty contents <> semi
--     _                              -> pretty ("readonly " <> name <> ":")  <+> pretty contents <> semi
--
-- renderConstructorField :: TSField -> Doc ann
-- renderConstructorField (TSField name contents) = pretty ("this." <> name <> " = " <> name <> "Var")
--
--
-- stackFields :: (TSField -> Doc ann) -> [TSField] -> Doc ann
-- stackFields fieldRenderer fields = indentStack 4 (fieldRenderer <$> fields)
--
-- renderClassConstructorVars :: [TSField] -> Doc ann
-- renderClassConstructorVars fields =
--   let
--     grabField (TSField name contents) = pretty (name <> "Var:") <+> pretty contents
--   in
--     hsep $ punctuate comma (grabField <$> fields)
--
-- renderClassConstructor :: [TSField] -> Doc ann
-- renderClassConstructor fields = indent 4 (
--                              pretty ("constructor" :: Text)
--                           <> parens (renderClassConstructorVars fields)
--                           <> hBraces (stackFields renderConstructorField fields)
--                              )
--
-- renderAlias :: TSDataType -> Doc ann
-- renderAlias (TSPrimitive _)         = emptyDoc
-- renderAlias (TSDataType title cons) = if Prelude.length cons < 2 then emptyDoc
--                                       else
--                                         let alias = hsep (punctuate (space <> pipe) $ pretty <$> cons)
--                                         in pretty ("export type " <> title) <+> equals <+> alias <> hardline
--
-- renderClass :: TSConstructor -> Doc ann
-- renderClass (TSConstructor txt fields) = pretty ("export class " <> txt)
--                                         <+> hBraces (stackFields renderField fields <> hardline <> renderClassConstructor fields)
--                                         <> hardline
--
getDartType :: (DartTypeConvertible a) => Proxy a -> Doc ann
getDartType prx = pretty $ toDartType prx
