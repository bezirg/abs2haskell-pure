module Main where

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

main_ :: Method
main_ [] this wb k = \ () ->
                     Assign  "x" New $ \ () ->
                         Assign "f" (Async "x" m1 []) $ \ () ->
                             Assign"y" (Get "f") k


m1 :: Method
m1 [] this wb k = \ () ->
                  Assign "z" New $ \ () ->
                      While ("z" `BEq` "z") 
                                (\ k' -> Skip k') $ \ () ->
                                    Return "z" wb k


main :: IO ()
main = runIO 10000 main_ >>= print

{- passes, diverges, as it should
reached max steps
Heap: {
Objects: fromList [(0,fromList [("f",3),("x",2)]),(2,fromList [("z",4)]),(4,fromList [])]
Fut: fromList [(1,Nothing),(3,Nothing)]
Counter: 5}
it :: ()
 -}



