module Ex1 where

import ABS

{-
main = {
  t:=this
  x:=new
  y:=x
}
-}

main = run $ do
  writeAttr "t" thisObject
  writeAttr "x" newObject
  writeAttr "y" (readAttr "x")


{- passes, output
(2,
Heap (fromList [(0,fromList [("t",0),("x",1),("y",1)]),(1,fromList [])]) (fromList []),
0)
-}
