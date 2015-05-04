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
main = runIO 50 main_ >>= print

{- passes, output
Counter: 5

ObjectHeap: (fromList [(0,fromList [("f1",2),("f2",3),("x",1),("y",4)]),(1,fromList [("k",4),("r1",4),("z",4)]),(4,fromList [])]) 

FutureHeap: (fromList [(2,Just 4),(3,Just 4)])

ThisObject,0)
 -}
