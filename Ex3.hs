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

main = run $ \ this -> do
  writeAttr this "x" newObject
  writeAttr this "f" (async (readAttr this "x") m1)
  writeAttr this "y" (getFuture (readAttr this "f"))

m1 this = do
  writeAttr this "z" newObject
  while (readAttr this "z" === readAttr this "z") $ do
       skip
  stop (readAttr this "z")

{- passes, diverges, as it should -}
