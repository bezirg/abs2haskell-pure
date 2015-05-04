module Main where

import ABS

{-
main = {
  t:=this
  x:=new
  y:=x
}
-}

main_ :: Method
main_ [] this wb k = \ () -> 
                     Assign "t" (Param this) $ \ () ->
                         Assign "x" New $ \ () ->
                             Assign "y" (Attr "x") k
main :: IO ()
main = runIO 10 main_ >>= print

{- passes, output
finished (empty proctable), 6steps left
Heap: {
Objects: fromList [(0,fromList [("t",0),("x",2),("y",2)]),(2,fromList [])]
Fut: fromList [(1,Nothing)]
Counter: 3}
-}
