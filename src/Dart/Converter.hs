{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Dart.Converter where

import           Data.Proxy
import           Data.Text
import           GHC.Generics

-- | 'DartPrimitive' defines the built-in types in Dart
data DartPrimitive = DartBool
                   | DartInt
                   | DartDouble
                   | DartString
                   -- | DartMap
                   -- | DartRune
                   -- | DartSymbol
                   -- | DartOptional DartTypeLabel
                   | DartList DartTypeLabel
                   deriving(Show, Eq)

{-|
'DartDataType' describes the Haskell type to be exported to Dart.
This exported type can either be a directly translated to a Dart primitive
or it is an algebraic data type with a name and a set of constructors
-}
data DartDataType = DartPrimitive DartPrimitive
                  | DartDataType Text [ DartConstructor ]
                  deriving(Show, Eq)

-- | A data type to describe record fields of Haskell data types
data DartField = DartField Text DartTypeLabel deriving(Show, Eq)

-- | A representation of a Haskell constructor including the name and the fields
data DartConstructor = DartConstructor Text [ DartField ] deriving(Show, Eq)

{-|
A type label used in the record fields. The difference between this and
'DartDataType' is that a label only carries the type, not the constructors or
fields.
-}
data DartTypeLabel = DartPrimitiveLabel DartPrimitive
                   | DartLabel Text
                   deriving(Show, Eq)


{-|
A function which produces a type label given a Haskell type
-}
getTypeLabel :: DartTypeConvertible a => Proxy a -> DartTypeLabel
getTypeLabel prx =
 case toDartType prx of
   DartPrimitive primitive -> DartPrimitiveLabel primitive
   DartDataType name _     -> DartLabel name

------------------------------

{-|
Class for converting Haskell types to Dart types.
Primitives have a straightforward definition.
Non-primitives rely on having Generics derived and are constructed using the
default implementation of "toDartType" function
-}

class DartTypeConvertible a where
  toDartType :: a -> DartDataType
  toDartType = genericToDartType . from
  default toDartType :: (Generic a, GenericToDartType (Rep a)) => a -> DartDataType

{-|
Convenience function: allows to use Proxy instead of undefined
-}
instance (DartTypeConvertible a) =>
  DartTypeConvertible (Proxy a) where
  toDartType _ = toDartType (undefined :: a)

{-|
Conversion of Text to Dart String
-}
instance DartTypeConvertible Text where
  toDartType _ = DartPrimitive DartString

{-|
Conversion of String to Dart String
-}
instance DartTypeConvertible String where
  toDartType _ = DartPrimitive DartString


{-|
Conversion of Float to Dart Double
-}
instance DartTypeConvertible Float where
  toDartType _ = DartPrimitive DartDouble

{-|
Conversion of Double to Dart Double
-}
instance DartTypeConvertible Double where
  toDartType _ = DartPrimitive DartDouble

{-|
Conversion of Int to Dart Int
-}
instance DartTypeConvertible Int where
  toDartType _ = DartPrimitive DartInt

{-|
Conversion of Bool to Dart Bool
-}
instance DartTypeConvertible Bool where
  toDartType _ = DartPrimitive DartBool

-- instance DartTypeConvertible a =>
--          DartTypeConvertible (Maybe a) where
--   toDartType _ = DartPrimitive (DartOptional (getTypeLabel (Proxy :: Proxy a)))

{-|
Conversion of List to Dart List
-}
instance (DartTypeConvertible a) =>
         DartTypeConvertible [a] where
  toDartType _ = DartPrimitive (DartList (getTypeLabel (Proxy :: Proxy a)))

------------------------------

{-|
Typically we will be converting Haskell data types with Generics derived.
GenericToDartType class defines how one does that
-}
class GenericToDartType a where
  genericToDartType :: a c -> DartDataType

instance (Datatype d, GenericToDartConstructor f) =>
         GenericToDartType (D1 d f) where
  genericToDartType dt =
    DartDataType (pack $ datatypeName dt) (genericToDartConstructor $ unM1 dt)

------------------------------

{-|
Class for obtaining constructors from the Generic class
-}
class GenericToDartConstructor a where
  genericToDartConstructor :: a c -> [ DartConstructor ]

instance (Constructor c, GenericToDartField f) =>
          GenericToDartConstructor (C1 c f) where
  genericToDartConstructor c =
      let
        constructorName = (pack $ conName c)
        constructorContents = (genericToDartField $ unM1 c)
      in
        [ DartConstructor constructorName constructorContents ]

instance (GenericToDartConstructor a, GenericToDartConstructor b) =>
          GenericToDartConstructor (a :+: b) where
  genericToDartConstructor _ =
    genericToDartConstructor (undefined :: a p) ++
    genericToDartConstructor (undefined :: b p)

------------------------------

{-|
Class for obtaining fields from the Generic class
-}
class GenericToDartField a where
  genericToDartField :: a p -> [ DartField ]

instance GenericToDartField U1 where
  genericToDartField _ = []

instance (GenericToDartField f, GenericToDartField g) =>
          GenericToDartField (f :*: g) where
  genericToDartField _ =
      genericToDartField (undefined :: f p) ++
      genericToDartField (undefined :: g p)

instance (Selector s, DartTypeConvertible a) =>
          GenericToDartField (S1 s (Rec0 a)) where
  genericToDartField selector =
    if selName selector == "" then
        error "All non-nullary data type constructors must have named field selectors"
    else
      case toDartType (Proxy :: Proxy a) of
        DartPrimitive primitive -> [DartField (pack $ selName selector) (DartPrimitiveLabel primitive)]
        DartDataType name _     -> [DartField (pack $ selName selector) (DartLabel name)]
