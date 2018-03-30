{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples where

import           Dart
import           Data.Proxy
import           Data.Text                             as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           GHC.Generics

  ----------------------------
  -- DEMO --


data Animal = Dog | Cat | Bird{limbs :: Int, wings :: Int} | Dunky{foots :: Text, eyz :: Int} deriving(Show, Generic, Eq, DartTypeConvertible)

data Pet = Pet {animal ::  [ Animal ]} deriving(Show, Generic, Eq, DartTypeConvertible)

-- data TupleTrial = TupleTrial {trial :: (Animal, Pet, Pet,Animal, Pet)} deriving(Show, Generic, Eq, DartTypeConvertible)

trialSpec :: Spec ann
trialSpec = Spec ["TestFolder", "testing"] [getDartType (Proxy :: Proxy Animal)
                                          , getDartType (Proxy :: Proxy Pet)]


testSaving :: IO()
testSaving = specToFile "ParentTest" trialSpec
