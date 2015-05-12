module Main where

import ABS

{-
main = {
  x := new;
  f := x ! m1();
  y = f.get;
}

m1 = {
  z := new;
  return z;
}

-}

(x:f:y:z:_) = [1..]

main_ :: Method
main_ [] this wb k = \ () ->
  Assign x New $ \ () ->
    Assign f (Async x m1 []) $ \ () ->
        Assign y (Get f) k

m1 :: Method
m1 [] this wb k = \ () ->
  Assign z New $ \ () ->
      Return z wb k

main :: IO ()
main = printHeap =<< run 10 main_

{- passes, output
finished (empty schedtable), 3steps left
Heap: {
    Objects:(4,(fromList [],fromList []))(2,(fromList [(4,4)],fromList []))(0,(fromList [(0,-123),(1,2),(2,3),(3,4)],fromList []))
    Futures:(3,Just 4)(1,Just (-123))
    Counter: 5
}
-}
