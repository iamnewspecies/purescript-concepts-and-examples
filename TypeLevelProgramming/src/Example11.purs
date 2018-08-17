module TLP.Example11 where

import Data.Foldable
import Data.Monoid (mempty)
import Control.Applicative (pure)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude (show, ($), (<$>), (<>), Unit, unit, discard)
import Type.Row (kind RowList, Nil, Cons)
import Record (get)
import Prim.Row (class Cons)
import Type.Data.RowList (RLProxy(..))
import Prim.RowList (class RowToList)
import Data.Array ((:))
import Effect (Effect)
import Effect.Console (log)


-- import TLP.Example08 (class IsSymbol)

-- Need to print all the keys

-- Need get hold of all the keys using type class constraints
    -- need to get the rowlist version of the record.
-- Use SProxy to get all the keys and then use reflectSymbol to print all the symbol.


-- Here we will write a program to print a record in purescript.

-- First we need to write a way to generate the String output of each code.
class WriteJson a where
    writeJson :: a -> String

-- This is for String
instance stringWriteJson :: WriteJson String where
    writeJson s = s

-- This is for Int
instance intWriteJson :: WriteJson Int where
    writeJson i = show i

-- All the data type are not covered. As this is waste time.

-- This is for array.
instance arrayWriteJson :: WriteJson a => WriteJson (Array a) where
	writeJson arr = "["<> contents <>"]"
		where
			contents = intercalate "," $ writeJson <$> arr

-- After this step we have to write the fuction which can parse json data.
-- Which basically means to write keys and values. 
-- Values can be printed from the above code.
-- Following code somehow has to get the keys. And from that it has to get the values.
-- What is the motivation for row list. 
-- Is it like to get way to iterate over the keys of the keys?

-- What is rowList (rl)
	-- The list of all the key value pairs.
-- What is row?
	-- One row of key and value. Whrere key is the Symbol and value is kind.
-- Why do you need rl -> row constarints?
	-- The row is decided if rl is decided
-- What does Record row mean?
	-- Record is type in Prim library.
-- type Person = Record (name :: String, age :: Number)
	-- "{..}" is syntactic sugar for "Record (...)"

-- What is the meaning of signature of writeJSONFields
	-- the function takes two inputs 
-- What is the meaning of "g rl". Does g take rl as an input parameter? Or is it a way of matching with data type
-- What is g and rl?
	-- Is g like a data constructor?
		-- g is like a proxy. Which holds the symbol.

class WriteJSONFields (rl :: RowList) row | rl -> row where
	writeJSONFields :: forall g. g rl -> Record row -> Array String

-- As rowlist is a recursive data-structure we need a nil for the end case
instance nilWriteJSONFields :: WriteJSONFields Nil row where
	writeJSONFields _ _ = mempty

-- Now we need to recusrsively do it for each element of the rowList
	-- Now if we want to recursively do it then we need a way to do the 
-- IsSymbol name -- is putting a constraint on name which will be used outside.
-- ty is a constraint that is being put 

-- here :: (IsSymbol name, WriteJson ty, WriteJSONFields tail row, Cons name ty whatever row)
-- the cons in the above mentioned line is type class and Cons "=> WriteJSONFields (Cons name ty tail) row" 
-- is a type and not type class.

instance consWriteJSONFields :: (
		IsSymbol name, 
		WriteJson ty, 
		WriteJSONFields tail row, 
		Cons name ty whatever row
	) => WriteJSONFields (Cons name ty tail) row where
	writeJSONFields _ rec = head : rest
		where
			namep = SProxy :: SProxy name
			key = reflectSymbol namep
			value = writeJson $ get namep rec
			head = "\"" <> key <> "\" : " <> value
			tailP = RLProxy :: RLProxy tail
			rest = writeJSONFields tailP rec


-- we need something which changes Record to RowList 
-- instance recordWriteJSONFields 

instance recordWriteJSON :: 
	(
		RowToList row rl,
		WriteJSONFields rl row
	) => WriteJson (Record row) where
		writeJson rec = "{" <> fields <>"}"
			where
				rlp = RLProxy :: RLProxy rl
				fields = intercalate "," $ writeJSONFields rlp rec

-- Print all keys of the data A
-- how do you print B and C
-- data A = B String | C String

main :: Effect Unit
main = do
	log $ writeJson {a : "hi", b : 123, c : { d : 123}}
	pure unit