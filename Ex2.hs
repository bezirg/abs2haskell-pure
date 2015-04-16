module Ex2 where

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

main = run $ \ this -> do
  writeAttr this "x" newObject
  writeAttr this "f" (async (readAttr this "x") m1)
  writeAttr this "y" (getFuture (readAttr this "f"))

m1 this = do
  writeAttr this "z" newObject
  return_ (readAttr this "z")

{- passes, output
(4,
ObjHeap (fromList [(0,fromList [("f",2),("x",1),("y",3)]),(1,fromList [("z",3)]),(3,fromList [])]) 
FutHeap (fromList [(2,Just 3)])
,0)
-}
