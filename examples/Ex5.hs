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
main = runIO 10 main_ >>= print


{- passes, output
finished (empty proctable), 1steps left
Heap: {
Objects: fromList [(0,fromList [("f",3),("x",4),("y",2),("z",4)]),(2,fromList [("z",4)]),(4,fromList [])]
Fut: fromList [(1,Nothing),(3,Just 4)]
Counter: 5}
-}
