module Main where

import ABS

{-

main = {
   "x" = m1();
}

m1 = {
  "y" = New;
  "f" = "y" ! m2 ();
  "z" = "f".get;
  return "z";
}

m2 = {
  "z" = New;
  return "z";
}

-}

(x:y:f:z:_) = [1..]

main_ :: Method
main_ [] this wb k = \ () ->
                     Assign x (Sync m1 []) k

m1 :: Method
m1 [] this wb k = \ () ->
                  Assign y New $ \ () -> 
                      Assign f (Async y m2 []) $ \ () ->
                          Assign z (Get f) $ \ () ->
                              Return z wb k

m2 :: Method
m2 [] this wb k = \ () ->
                  Assign z New $ \ () ->
                      Return z wb k

main :: IO ()
main = printHeap =<< run 11 main_


{- passes, output
finished (empty schedtable), 2steps left
Heap: {
    Objects:(4,(fromList [],fromList []))(2,(fromList [(4,4)],fromList []))(0,(fromList [(0,-123),(1,4),(2,2),(3,3),(4,4)],fromList []))
    Futures:(3,Right 4)(1,Right (-123))
    Counter: 5
}
finished (empty schedtable), 2steps left
Heap: {
    Objects:(4,(fromList [],fromList []))(2,(fromList [(4,4)],fromList []))(0,(fromList [(0,-123),(1,4),(2,2),(3,3),(4,4)],fromList []))
    Futures:(3,Right 4)(1,Right (-123))
    Counter: 5
}
-}
