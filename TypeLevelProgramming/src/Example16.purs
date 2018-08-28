module TLP.Example16 where

import Prelude (($), Unit)
import Effect.Console (log)
import Effect (Effect)

-- This SProxy is a way to push types into value level programing. It is not the other way around.
-- Values of imput and output at type level programming has to be predetermined.

data TSA (a :: Symbol) = TSA

class TestSymbols a where
    execute :: TSA a -> String

instance oneTestSymbols :: TestSymbols "One" where
    execute _ = "I am one"

instance otherTestSymbols :: TestSymbols "Other" where
    execute _ = "I am other"

instance twoTestSymbols :: TestSymbols "Two" where
    execute _ = "I am two"

main :: Effect Unit
main = log $ execute (TSA :: TSA "Other")
