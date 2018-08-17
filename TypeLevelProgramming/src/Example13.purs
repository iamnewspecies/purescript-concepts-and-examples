module TLP.Example13 where

-- This goal is to print all the keys of a record

import Data.Foldable (intercalate)
import Data.Functor (map)
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude (show, (<>), ($), Unit, unit, bind, pure)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList)
import Type.Data.RowList (RLProxy(..))
import Type.Row (kind RowList, Cons, Nil)
import Data.Array ((:))
import Effect (Effect)
import Effect.Console (log)
import Record (get)

class WriteJSON a where
  writeJSON :: a -> String

instance intWriteJSON :: WriteJSON Int where
  writeJSON a = show a

instance stringWriteJSON :: WriteJSON String where
  writeJSON s = s

instance arrayWriteJSON :: (WriteJSON a) =>  WriteJSON (Array a) where
  writeJSON a = "[" <> values <> "]"
    where
      values = intercalate "," $ map writeJSON a

-- We cannot write json unless we have access to values
-- We can do this if convert it into RowList.

instance recordWriteJSON :: 
  -- some constraints so that we are able to print all the elements of the field
  -- RowToList helps adding what else is going to happen to the RowToList
  (
    RowToList row list -- this conversion happens on its own
  , WriteJSONFields row list -- Can the order of this and the constraint before this be reversed and still it works?
  ) => WriteJSON (Record row) where
  writeJSON rec = "{" <> contents <> "}"
    where
      rlp = RLProxy :: RLProxy list
      contents = intercalate "," $ writeJSONFields rlp rec

-- now we need something to iterate over all the contents of 
-- g is there for proxy
-- What is the rl -> row connected to?
class WriteJSONFields row (rl :: RowList) | rl -> row  where
  writeJSONFields :: forall g. g rl -> Record row -> Array String

-- row can be row as we have put rl -> row constraint on the class signature. But interestingly it compiles without that.
instance nilWriteJSONFields :: WriteJSONFields row Nil where
  writeJSONFields _ _ = mempty

instance recordWriteJSONFields :: (
    IsSymbol name -- this will help us get a SProxy out the name
  , WriteJSON ty -- This works for a String
  , WriteJSONFields row tail
  , Cons name ty whatever row -- class signature of Cons is name -> type -> RowList -> RowList. How is the type of RowList same as row?
  ) => WriteJSONFields row (Cons name ty tail) where
  writeJSONFields _ rec = head : rest
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      -- Note :: get is defined in terms of namep
      value = writeJSON $ get namep rec
      head = "\"" <> key <> "\" : " <> value -- this is important so that it goes in recursively.
      tailp = RLProxy :: RLProxy tail
      rest = writeJSONFields tailp rec

-- row = (a : "456", d : "123", c : "789")
-- Cons c String (Cons d String (Cons a String (Nil)))


main :: Effect Unit
main = do
  _ <- log $ writeJSON { a : "456", d : "123", c : "789" }
  pure unit

myRec :: Record Row1
myRec = { a : "sdd", b : 2, c : true}

type Row1 = (a :: String, b :: Int, c :: Boolean)
type Whatever = (a :: String, b :: Int)