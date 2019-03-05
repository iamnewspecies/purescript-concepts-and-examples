module TLP.Example17 where

import Data.Symbol
import Prelude
import Effect.Console
import Data.String.Common
import Effect.Random

-- https://github.com/paf31/24-days-of-purescript-2016/blob/master/9.markdown

-- reifySymbol :: forall r. String -> (forall sym. IsSymbol sym => SProxy sym -> r) -> r
-- reifySymbol s f = coerce f { reflectSymbol: \_ -> s } SProxy where
--   coerce
--     :: (forall sym1. IsSymbol sym1              => SProxy sym1 -> r)
--     -> { reflectSymbol :: SProxy "" -> String } -> SProxy ""   -> r
--   coerce = unsafeCoerce

-- What functionality do we have to ensure at compile time if the symbol is coming from runtime?
    -- Is this function impure?
        -- Unsafecoerce is being used on this
    -- reflect symbol should work.
    -- SProxy on it should work.

foreign import kind PG

data PGProxy (sym :: PG) = PGProxy

newtype Sep (s :: Symbol) = Sep (Array String)

sep :: forall s. String -> Sep s
sep s = Sep [s]

derive newtype instance semigroupSep :: Semigroup (Sep s)
derive newtype instance monoidSep :: Monoid (Sep s)

renderSep :: forall s. IsSymbol s => Sep s -> String
renderSep (Sep items) = let sep = reflectSymbol (SProxy :: SProxy s)
                        in joinWith sep items

comeFrom :: forall s. IsSymbol s => SProxy s -> String
comeFrom _ = renderSep (sep "I come from " <> sep "!!!" :: Sep s)

extraFuntion r = (reifySymbol r comeFrom)

-- Getting a type from runtime
main = do
  int <- randomInt 1 10
  log (extraFuntion (show int))
  log (comeFrom (SProxy :: SProxy "compile"))