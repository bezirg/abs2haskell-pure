module Ex3 where

import ABS

{-
main = {
  x := new;
  f := x ! m1();
  y = f.get;
}

m1 = {
  z := new;
  while (z==z) {
      skip;
    }
  return z;
}

-}

main = run $ do
  writeAttr "x" newObject
  writeAttr "f" (async (readAttr "x") m1)
  writeAttr "y" (getFuture (readAttr "f"))

m1 = do
  writeAttr "z" newObject
  while (readAttr "z" === readAttr "z") $ do
       skip
  stop (readAttr "z")

{- passes, diverges, as it should -}
