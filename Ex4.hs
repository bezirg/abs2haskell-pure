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

main = run $ do
  writeAttr "x" newObject
  writeAttr "f1" (async (readAttr "x") m1)
  writeAttr "f2" (readAttr "f1" >>= \ f1 -> async (readAttr "x") (m2 f1))
  writeAttr "y" (getFuture (readAttr "f2"))

m1 = do
  writeAttr "z" newObject
  stop (readAttr "z")

m2 p1 = do
  writeAttr "r1" (getFuture (return p1))
  ifM (readAttr "z" === readAttr "r1") (writeAttr "k" (readAttr "r1")) (writeAttr "k" thisObject)
  stop (readAttr "k")

{- passes, output
Counter: 5

ObjectHeap: (fromList [(0,fromList [("f1",2),("f2",3),("x",1),("y",4)]),(1,fromList [("k",4),("r1",4),("z",4)]),(4,fromList [])]) 

FutureHeap: (fromList [(2,Just 4),(3,Just 4)])

ThisObject,0)
 -}
