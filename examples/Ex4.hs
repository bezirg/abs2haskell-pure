{-# LANGUAGE ImplicitParams #-}

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

attrs@(x:f1:f2:y:z:p1_:r1:w:[]) = [1..8]

main_ :: Method
main_ [] this wb k = 
                     assign x New $ 
                         assign f1 (Async x m1 []) $ 
                             assign f2 (Async x m2 [f1]) $ 
                                 assign y (Get f2) k

m1 :: Method
m1 [] this wb k = 
                  assign z New $ 
                      return_ z wb k

m2 :: Method
m2 [p1] this wb k = 
                    assign p1_ (Param p1) $                      -- aux attr
                        assign r1 (Get p1_) $ 
                            if_ (z `BEq` r1) (\ k' -> assign w (Attr r1) k') (\ k' -> assign w (Param this) k') $ 
                                return_ w wb k

main :: IO ()
main = printHeap =<< run 50 main_ (length attrs+1)

{- passes, output
finished (empty schedtable), 37steps left
Heap: {
    Objects:(5,(fromList [],fromList []))(2,(fromList [(5,5),(6,3),(7,5),(8,5)],fromList []))(0,(fromList [(0,-123),(1,2),(2,3),(3,4),(4,5)],fromList []))
    Futures:(4,Right 5)(3,Right 5)(1,Right (-123))
    Counter: 6
}
-}
