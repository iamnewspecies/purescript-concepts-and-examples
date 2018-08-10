module TLP.Example01 where

import Prelude
import Data.Maybe (Maybe(..), fromJust)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Partial.Unsafe (unsafePartial)

-- Prim library

-- Function
-- Core Data Structures
    -- Array
    -- Record
-- Core data types
    -- Number
    -- Int
    -- String
    -- Char
    -- Boolean
-- Partial - We will look into this


-- Is Partial a type classes
-- Are there typeclasses which don't take parameters

mainCompileTime :: Partial => Effect Unit
mainCompileTime = logShow (fromJust (Just 3))

mainRunTime :: Effect Unit
mainRunTime = logShow (unsafePartial (fromJust (Just 3)))

-- Trying to bring my own kind to purescript which we will call RowList
foreign import kind RowList

-- Will define "Nil" type of kind RowList
foreign import data Nil :: RowList

-- Cons is a function in type world which takes kinds Symbols, type and RowList to give a new RowList.
foreign import data Cons :: Symbol -> Type -> RowList -> RowList

-- A Symbol is a key and Type is

-- This is a typeclass at type level. 
class RowToList (row :: # Type) (list :: RowList) | row -> list

-- How to write an instance of this?

-- instance rowToListNil :: RowToList () Nil
-- instance rowToListSingle :: RowToList (a :: A) (Cons "a" A Nil)
-- instance rowToListDouble  :: RowToList (a :: A, b :: B) (Cons "b" A (Cons "b" B Nil))
-- instance rowToListTriple :: RowToList (a :: A, b :: B, c :: C) (Cons "c" C (Cons "b" B (Const "a" A Nil)))

-- Reverse of List to row
class ListToRow (list :: RowList) (row :: # Type) | list -> row

-- The base case is here
instance listToRowNil :: ListToRow Nil ()

-- What is the use of this step?
-- Why is the compiler not able to find RowCons?
-- instance listToRowCons :: (ListToRow tail tailRow, RowCons label ty tailRow row ) => ListToRow (Cons label ty tail) row

-- | Example of RowCons

-- applyRecord :: forall io i o. ApplyRecord io i o => Record io -> Record i -> Record o

class ApplyRecord (io :: # Type) (i :: # Type) (o :: # Type) | i o -> io, io o -> i, io i -> o

-- Everything on left as RowToList.
-- ApplyRowList is the center
-- ListToRow is on the right
instance applyRecordImpl :: ( RowToList io lio, RowToList i li, RowToList o lo, ApplyRowList lio li lo, ListToRow lio io, ListToRow li i, ListToRow lo o ) => ApplyRecord io i o

class ApplyRowList (io :: RowList) (i :: RowList) (o :: RowList) | i o -> io, io o -> i, io i -> o

-- What next?