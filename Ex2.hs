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

main = run $ do
  writeAttr "x" newObject
  writeAttr "f" (async (readAttr "x") m1)
  writeAttr "y" (getFuture (readAttr "f"))

m1 = do
  writeAttr "z" newObject
  stop (readAttr "z")

{- passes, output
(4,
ObjHeap (fromList [(0,fromList [("f",2),("x",1),("y",3)]),(1,fromList [("z",3)]),(3,fromList [])]) 
FutHeap (fromList [(2,Just 3)])
,0)
-}
