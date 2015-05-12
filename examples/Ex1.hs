module Main where

import ABS

{-
main = {
  t:=this
  x:=new
  y:=x
}
-}

(t:x:y:_) = [1..]

main_ :: Method
main_ [] this wb k = \ () -> 
                     Assign t (Param this) $ \ () ->
                         Assign x New $ \ () ->
                             Assign y (Attr x) k
main :: IO ()
main = printHeap =<< run 10 main_

{- passes, output
finished (empty schedtable), 6steps left
Heap: {
    Objects:(2,(fromList [],fromList []))(0,(fromList [(0,-123),(1,0),(2,2),(3,2)],fromList []))
    Futures:(1,Just (-123))
    Counter: 3
}
-}
