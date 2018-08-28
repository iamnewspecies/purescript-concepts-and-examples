module TLP.Example15 where

import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, unit, ($), pure, show, (>), bind)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Record (get)
import Record.Builder (build, insert)
import Type.Data.RowList (RLProxy(..))
import Type.Row (class ListToRow, kind RowList, Cons, Nil)
import Unsafe.Coerce (unsafeCoerce)

class TypeConverter (i :: # Type) (o :: # Type) where
    to :: Record i -> Record o

-- how do I say o is sub set of i. Will type system automatically check that?
instance recordTypeConverter :: (
        RowToList i il
    ,   RowToList o ol
    ,   TypeConverterRowList il i ol o
    ,   ListToRow ol o
    ) => TypeConverter i o where
    to reci = reco
        where
            olp = RLProxy :: RLProxy ol
            ilp = RLProxy :: RLProxy il
            reco = extract ilp reci olp

class TypeConverterRowList (il :: RowList) rowi (ol :: RowList) rowo where
    extract :: forall g h. g il -> Record rowi -> h ol -> Record rowo

instance nilTypeConverter :: TypeConverterRowList (Cons ki tyi taili) rowi Nil rowo where
    extract _ reci _ = unsafeCoerce {}

instance rowListTypeConverter :: (
        IsSymbol ko
    ,   IsSymbol ki
    ,   TypeConverterRowList (Cons ki tyi taili) rowi tailo whatevero
    ,   Cons ko tyo whatevero rowo
    ,   Cons ko tyo whateveri rowi -- all the keys in the first have to be there in the second.
    ,   Cons ki tyi whateveri rowi
    ,   Lacks ko whatevero
    ) => TypeConverterRowList (Cons ki tyi taili) rowi (Cons ko tyo tailo) rowo where
    extract ilp reci _ = reco
        where
            keyp = SProxy :: SProxy ko
            value = get keyp reci

            olp = RLProxy :: RLProxy tailo
            
            subseto = extract ilp reci olp --  {}
            reco = build (insert keyp value) subseto -- {a : }

bar :: {a :: Boolean, b :: Int}
bar = {a: true, b: 0}

someFunction 
	:: TypeConverter 
			(a :: Boolean, b:: Int) 
			(a :: Boolean)
	=> Record (a :: Boolean)
someFunction = to bar

-- getSubset :: forall x. IsTypeSubset x y => y -> x
-- getSubset y = unsafeCoerce unit

-- this code will generate the subset of the given type
main :: Effect Unit
main = do  
	_ <- log $ show $ someFunction
	pure unit