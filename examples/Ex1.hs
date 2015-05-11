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
main = print =<< run 10 main_

{- passes, output
finished (empty schedtable), 6steps left
Heap: {
Objects: fromList [(0,(fromList [("__main__",-123),("t",0),("x",2),("y",2)],fromList [])),(2,(fromList [],fromList []))]
Fut: fromList [(1,Just (-123))]
Counter: 3}
-}
