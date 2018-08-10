module TLP.Example08 where

import Data.Monoid
import Data.Semigroup
import Data.String.Common

import Prelude (discard)
import Effect.Console (log)
import Data.Symbol

-- | 9. Symbols and IsSymbol Instances

-- What are Symbols?
    -- "iamawesome" -> String -> *
    --                 "iamawesome" -> Symbol
        -- A string a type level has kind Symbol
-- Why do you need this?
    -- Symbols were originally introduced for the Fail type class - this allows writing custom error messages in types for when someone tries to use a type class for some specific instance that isn't supported.

-- What is the use of IsSymbol instance?
-- Why do you need them together? (Structure and movement)


-- | Test

-- Here following is just used to carry around the symbol information.

-- | 9. Symbols and IsSymbol Instances

-- What are Symbols?
    -- "iamawesome" -> String -> *
    --                 "iamawesome" -> Symbol
        -- A string a type level has kind Symbol
-- Why do you need this?
    -- Symbols were originally introduced for the Fail type class - this allows 
    -- writing custom error messages in types for when someone tries to use a type class for some specific instance that isn't supported.

-- What is the use of IsSymbol instance?
-- Why do you need them together? (Structure and movement)


-- | Test

-- Here following is just used to carry around the symbol information.
-- data SProxy (sym :: Symbol) = SProxy

-- following is the Prims definition typeConcat

-- How do we define at type level a function?
-- data TypeConcat = TypeConcat :: Symbol -> Symbol -> Symbol
-- which is defined as follows
-- infixl 6 type TypeConcat as <>


-- SProxy :: SProxy (TypeConcat "Merry" "Xmas")

-- isSymbol is typeClass
-- class IsSymbol (sym :: Symbol) where
--   reflectSymbol :: SProxy sym -> String

-- reflectSymbol is already defined
-- we can reuse and do some type level programming to generate the code
-- instance concatenateSymbols :: (IsSymbol left, IsSymbol right) => IsSymbol (TypeConcat left right) where
--     reflectSymbol _ = (SProxy left :: SProxy ) <> (SProxy right :: SProxy)
-- g = reflectSymbol (SProxy :: SProxy "tinsel")


-- | Steps the above code will go through when it has to generate a code.
-- reflectSymbol (SProxy :: SProxy ("coo" <> "kies"))
-- -- ^ starting expression

-- reflectSymbol (SProxy :: SProxy "coo") <> reflectSymbol (SProxy :: SProxy "kies")
-- -- ^ by the definition of reflectSymbol for TypeConcat

-- "coo" <> reflectSymbol (SProxy :: SProxy "kies")
-- -- ^ by the definition of reflectSymbol for the symbol "coo"

-- "coo" <> "kies"
-- -- ^ by the definition of reflectSymbol for the symbol "kies"

-- "cookies"
-- -- ^ a delicious result


-- | Example
-- this takes a symbol and returns a String of kind.
-- Sep :: Symbol -> *

-- don't know what this is really doing
-- sep :: forall s. String -> Sep s

-- | renderSep :: forall s. IsSymbol s => Sep s -> String

-- first we need to define Sep then
newtype Sep (s :: Symbol) = Sep (Array String)

-- next we define a sep function which makes a string 
-- into something which needs to be added to the
sep :: forall s. String -> Sep s
sep s = Sep [s]

-- What are these concepts?
-- It seems they are required for append an array etc.
-- What is the other use case of this?
-- Need to look into the papers for this.
derive newtype instance semigroupSep :: Semigroup (Sep s)
derive newtype instance monoidSep :: Monoid (Sep s)

renderSep :: forall s. IsSymbol s => Sep s -> String
renderSep (Sep items) = let sep = reflectSymbol (SProxy :: SProxy s)
                        in joinWith sep items


-- reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => SProxy sym -> r) -> r

comeFrom :: forall s. IsSymbol s => SProxy s -> String
comeFrom _ = renderSep (sep "I come from " <> sep "!!!" :: Sep s)

main = do
  log (reifySymbol "runtime" comeFrom)
  log (comeFrom (SProxy :: SProxy "compile"))

-- need to throughly understand this part where we make compile time to rutime gateway.
-- extensive use of constraints, typeclasses, newtype, data, Sproxy.
-- Didn't quite get what happened from the "renderSep" function

-- What next?