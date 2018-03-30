{-|
Basic usage:

@
import Dart

data Animal = Dog | Cat | Bird{limbs :: Int, wings :: Int} deriving(Show, Generic, Eq, DartTypeConvertible)
data Pet = Pet {animal ::  [ Animal ]} deriving(Show, Generic, Eq, DartTypeConvertible)

trialSpec :: Spec ann
trialSpec = Spec ["testing"] [getDartType (Proxy :: Proxy Animal)
                            , getDartType (Proxy :: Proxy Pet)]


main :: IO ()
main = specToFile "some-folder" trialSpec
@

This results in

@
abstract class Animal {
 final String _tag;

 Animal(this._tag)
}

class Dog extends Animal {

 Dog() : super('Dog');
}

class Cat extends Animal {

 Cat() : super('Cat');
}

class Bird extends Animal {
 final int limbs;
 final int wings;

 Bird(this.limbs, this.wings) : super('Bird');
}
class Pet {
 final List\<Animal\> animal;

 Pet(this.animal);
}
@

NOT SUPPORTED: "Maybe" types are currently not exported by this library
-}

module Dart (
      DartTypeConvertible
    , specToFile
    , Spec(..)
    , getDartType
    ) where

import           Dart.Converter (DartTypeConvertible)
import           Dart.File      (Spec (..), specToFile)
import           Dart.Types     (getDartType)
