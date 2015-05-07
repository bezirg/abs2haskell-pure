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
  return z;
}

-}

main_ :: Method
main_ [] this wb k = \ () ->
  Assign "x" New $ \ () ->
    Assign "f" (Async "x" m1 []) $ \ () ->
        Assign "y" (Get "f") k

m1 :: Method
m1 [] this wb k = \ () ->
  Assign "z" New $ \ () ->
      Return "z" wb k

main :: IO ()
main = print (run 10 main_)

{- passes, output
finished (empty proctable), 2steps left
Heap: {
Objects: fromList [(0,fromList [("__main__",-123),("f",3),("x",2),("y",4)]),(2,fromList [("z",4)]),(4,fromList [])]
Fut: fromList [(1,Just (-123)),(3,Just 4)]
Counter: 5}
-}
