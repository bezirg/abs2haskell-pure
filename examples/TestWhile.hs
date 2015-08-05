{-# LANGUAGE ImplicitParams #-}

module Main where

import ABS

counter:hundred:the_end:_=[1..]



main_ :: Method
main_ [] this wb k =
  assign counter (Param this) $
  assign hundred (Param 100) $
  while (BNeg (counter `BEq` hundred)) 
        (assign counter New) k

main :: IO ()
main = printHeap =<< run 10000000 main_ the_end
