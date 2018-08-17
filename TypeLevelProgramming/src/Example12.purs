module TLP.Example12 where

import Record
import Prim.RowList (class RowToList)
import Type.Row (kind RowList, Nil, class ListToRow, Cons)
import Prim.RowList (class RowToList)
import Prim.Row (class Cons)
import Prelude (show, (>))

-- https://liamgoodacre.github.io/purescript/rows/records/2017/07/10/purescript-row-to-list.html
-- liamgoodacre


-- Problem statement

-- What is the motivation in doing it at type level?
-- How would you do it if not at type level?

-- Interesting :: no need to write where clause
class ApplyRecord (io :: # Type) (i :: # Type) (o :: # Type) | i o -> io , i io -> o, o io -> i

-- Motivation of solving the applyRecord this way.
-- Such that each key in the input and the output may have distinct types

-- instance applyRowListNil
--   :: ApplyRowList Nil Nil Nil

instance applyRecordImpl
  :: ( RowToList io lio
     , RowToList i li
     , RowToList o lo
     , ApplyRowList lio li lo
     , ListToRow lio io
     , ListToRow li i
     , ListToRow lo o )
  => ApplyRecord io i o

class ApplyRowList (io :: RowList)
                   (i :: RowList)
                   (o :: RowList)
                   | i o -> io
                   , io o -> i
                   , io i -> o

instance applyRowListNil
  :: ApplyRowList Nil Nil Nil

instance applyRowListCons
  :: ApplyRowList tio ti to
  => ApplyRowList (Cons k (i -> o) tio) (Cons k i ti) (Cons k o to)

  -- setup

foo :: {a :: Boolean -> String, b :: Int -> Boolean}
foo = {a: show, b: (_ > 0)}

bar :: {a :: Boolean, b :: Int}
bar = {a: true, b: 0}

-- examples

-- applyRecord :: forall io i o. ApplyRecord io i o => Record io -> Record i -> Record o


-- eg0 x = applyRecord foo x
-- eg1 x = applyRecord x bar
-- eg2 r = applyRecord r.io r.i