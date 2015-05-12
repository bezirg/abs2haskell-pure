module Main where

import ABS

{-
main = {
  x := new;
  f1 := x ! m1();
  f2 := x ! m2(f1);
  y = f2.get;
}

m1 = {
  z := new;
  return z;
}

m2 p1 = {
  r1 := p1.get;
  if (z == r1) {
    w := r1;
  }
  else {
    w := this;
  }
  return w;
-}

(x:f1:f2:y:z:p1_:r1:w:_) = [1..]

main_ :: Method
main_ [] this wb k = \ () -> 
                     Assign x New $ \ () ->
                         Assign f1 (Async x m1 []) $ \ () ->
                             Assign f2 (Async x m2 [f1]) $ \ () ->
                                 Assign y (Get f2) k

m1 :: Method
m1 [] this wb k = \ () ->
                  Assign z New $ \ () ->
                      Return z wb k

m2 :: Method
m2 [p1] this wb k = \ () ->
                    Assign p1_ (Param p1) $ \ () ->                     -- aux attr
                        Assign r1 (Get p1_) $ \ () ->
                            If (z `BEq` r1) (\ k' -> Assign w (Attr r1) k') (\ k' -> Assign w (Param this) k') $ \ () ->
                                Return w wb k

main :: IO ()
main = printHeap =<< run 50 main_

{- passes, output
finished (empty schedtable), 33steps left
Heap: {
    Objects:(4,(fromList [],fromList []))(2,(fromList [(5,4),(6,3),(7,4),(8,4)],fromList []))(0,(fromList [(0,-123),(1,2),(2,3),(3,5),(4,4)],fromList []))
    Futures:(5,Just 4)(3,Just 4)(1,Just (-123))
    Counter: 6
}
-}
