module TLP.Example14 where

import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, unit, ($), pure, show, (>), bind)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Record (get)
import Record.Builder (build, insert, nub)
import Type.Data.RowList (RLProxy(..))
import Type.Row (class ListToRow, kind RowList, Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)

class ApplyRecord (i :: # Type) (o :: # Type) (io :: # Type) | i o -> io, i io -> o, io o -> i where
  applyRecord :: Record i -> Record io -> Record o

-- the problem here is inabilty to say what is the output type
class  ApplyRowList (i :: RowList) (rowi :: # Type) (o :: RowList) (rowo :: # Type) (io :: RowList) (rowio :: # Type) | i o -> io, io o -> i, io i -> o, i -> rowi, o -> rowo, io -> rowio  where
  applyRowList :: forall g. g i -> Record rowi -> g io -> Record rowio -> Record rowo

instance applyRecordlistInstance :: 
    (
      RowToList i li
    , RowToList o lo
    , RowToList io lio
    , ApplyRowList li i lo o lio io
    , ListToRow li i
    , ListToRow lo o
    , ListToRow lio io
    ) => ApplyRecord i o io where
    applyRecord ri rio = value
      where
        ip = RLProxy :: RLProxy li
        iop = RLProxy :: RLProxy lio
        value = applyRowList ip ri iop rio

instance applyNilRowListInstance :: ApplyRowList Nil rowi Nil rowo Nil rowio where
  applyRowList _ ri _ rio  = unsafeCoerce {}

instance applyRowListInstance :: (
    IsSymbol k
  , ApplyRowList ti rowi to whatevero tio rowio
  , Cons k i whateveri rowi
  , Cons k o whatevero rowo
  , Lacks k whatevero
  , Cons k (i -> o) whateverio rowio
  ) => ApplyRowList (Cons k i ti) rowi  (Cons k o to) rowo (Cons k (i -> o) tio) rowio where
    applyRowList _ ri _ rio = ro
      where 
        ip = RLProxy :: RLProxy ti
        iop = RLProxy :: RLProxy tio
        
        -- valueRecord :: Record whatevero
        valueRecord = (applyRowList ip ri iop rio)
        
        fp = SProxy :: SProxy k
        inputp = SProxy :: SProxy k
        
        f = (get fp rio)
        value = f (get inputp ri)
        ro = build (insert (SProxy :: SProxy k) value) valueRecord 

foo :: {a :: Boolean -> String, b :: Int -> Boolean}
foo = {a: show, b: (_ > 0)}

bar :: {a :: Boolean, b :: Int}
bar = {a: true, b: 0}

someFunction 
	:: forall rowo
	. ApplyRecord 
			(a :: Boolean, b :: Int) 
			rowo 
			(a :: Boolean -> String, b :: Int -> Boolean)
	=> Record rowo
someFunction = applyRecord bar foo


main :: Effect Unit
main = do  
	_ <- log $ show $ someFunction
	pure unit