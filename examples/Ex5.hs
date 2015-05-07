module Main where

import ABS

{-

main = {
   "x" = m1();
}

m1 = {
  "y" = New;
  "f" = "y" ! m2 ();
  "z" = "f".get;
  return "z";
}

m2 = {
  "z" = New;
  return "z";
}

-}


main_ :: Method
main_ [] this wb k = \ () ->
                     Assign "x" (Sync m1 []) k

m1 :: Method
m1 [] this wb k = \ () ->
                  Assign "y" New $ \ () -> 
                      Assign "f" (Async "y" m2 []) $ \ () ->
                          Assign "z" (Get "f") $ \ () ->
                              Return "z" wb k

m2 :: Method
m2 [] this wb k = \ () ->
                  Assign "z" New $ \ () ->
                      Return "z" wb k

main :: IO ()
main = print (run 11 main_)


{- passes, output
finished (empty schedtable), 2steps left
Heap: {
Objects: fromList [(0,(fromList [("__main__",-123),("f",3),("x",4),("y",2),("z",4)],fromList [])),(2,(fromList [("z",4)],fromList [])),(4,(fromList [],fromList []))]
Fut: fromList [(1,Just (-123)),(3,Just 4)]
Counter: 5}
-}
