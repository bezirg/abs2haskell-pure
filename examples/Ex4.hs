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
    k := r1;
  }
  else {
    k := this;
  }
  return k;
-}

main_ :: Method
main_ [] this wb k = \ () -> 
                     Assign "x" New $ \ () ->
                         Assign "f1" (Async "x" m1 []) $ \ () ->
                             Assign "f2" (Async "x" m2 ["f1"]) $ \ () ->
                                 Assign "y" (Get "f2") k

m1 :: Method
m1 [] this wb k = \ () ->
                  Assign "z" New $ \ () ->
                      Return "z" wb k

m2 :: Method
m2 [p1] this wb k = \ () ->
                    Assign "p1_" (Param p1) $ \ () ->                     -- aux attr
                        Assign "r1" (Get "p1_") $ \ () ->
                            If ("z" `BEq` "r1") (\ k' -> Assign "k" (Attr "r1") k') (\ k' -> Assign "k" (Param this) k') $ \ () ->
                                Return "k" wb k

main :: IO ()
main = print =<< run 50 main_

{- passes, output
finished (empty schedtable), 33steps left
Heap: {
Objects: fromList [(0,(fromList [("__main__",-123),("f1",3),("f2",5),("x",2),("y",4)],fromList [])),(2,(fromList [("k",4),("p1_",3),("r1",4),("z",4)],fromList [])),(4,(fromList [],fromList []))]
Fut: fromList [(1,Just (-123)),(3,Just 4),(5,Just 4)]
Counter: 6}
 -}
