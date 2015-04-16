module Ex4 where

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

main = run $ \ this -> do
  writeAttr this "x" newObject
  writeAttr this "f1" (async (readAttr this "x") m1)
  writeAttr this "f2" (do f1 <- readAttr this "f1"; async (readAttr this "x") (m2 f1))
  writeAttr this "y" (getFuture (readAttr this "f2"))

m1 this =  do
  writeAttr this "z" newObject
  return_ (readAttr this "z")

m2 p1 this = do
  writeAttr this "r1" (getFuture (return p1))
  ifM (readAttr this "z" === readAttr this "r1") (writeAttr this "k" (readAttr this "r1")) (writeAttr this "k" (return this))
  return_ (readAttr this "k")

{- passes, output
Counter: 5

ObjectHeap: (fromList [(0,fromList [("f1",2),("f2",3),("x",1),("y",4)]),(1,fromList [("k",4),("r1",4),("z",4)]),(4,fromList [])]) 

FutureHeap: (fromList [(2,Just 4),(3,Just 4)])

ThisObject,0)
 -}
