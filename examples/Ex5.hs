module Main where

import ABS

{-

main = {
   "x" = m1();
   "y" = x;
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

attrs@(x:y:f:z:_) = [1..4]

main_ :: Method
main_ [] this wb k = 
                     assign x (Sync m1 []) (assign y (Attr x) k)

m1 :: Method
m1 [] this wb k = 
                  assign y New $ 
                      assign f (Async y m2 []) $ 
                          assign z (Get f) $ 
                              return_ z wb k

m2 :: Method
m2 [] this wb k = 
                  assign z New $ 
                      return_ z wb k

main :: IO ()
main = printHeap =<< run 11 main_ (length attrs+1)


{- passes, output
finished (empty schedtable), 1steps left
Object Heap with array-size:10{
(4,(fromList [],fromList []))(2,(fromList [(4,4)],fromList []))(0,(fromList [(0,-123),(1,4),(2,4),(3,3),(4,4)],fromList []))
}
Future Heap with array-size:10{
(3,Right 4)(1,Right (-123))
}
    Counter: 5
-}
