module Test where

data A = A | B | C

someFunction :: A -> String
someFunction input = case input of 
    A -> "A"
    B -> "B"
    C -> "C"