module Main where

import Prelude
import Control.Monad.Eff (Eff)

-- There exist no such thing as Alphabets when you reach javascript
data Alphabets = A | B | C | D

-- 
newtype SomeNewtype = SomeNewtype String

getSomeAlphabet :: Alphabets
getSomeAlphabet = A

getSomeNewtype :: SomeNewtype
getSomeNewtype = SomeNewtype "Somenewtype"

main :: forall e. Eff e Unit
main = do
  c <- pure $ case getSomeAlphabet of
    A -> "A"
    B -> "B"
    C -> "C"
    D -> "D"
  SomeNewtype someNewType <- pure getSomeNewtype
  _ <- pure $ show ( c <> someNewType)
  pure unit