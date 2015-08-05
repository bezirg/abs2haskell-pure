{-# LANGUAGE ImplicitParams #-}

module Main where

import ABS

next:zero:r:g:the_end:_=[1..]

{-
main := this.go [InitialArgument]
-}

{-

go [current]this :=
  zero := 0;                     -- constant
  next := current - 1;
  if (next != zero) {
    l:=new;
    r:=new;
    f := l ! go(next);
    g := r ! go(next);
    await f;
    await g;
  }
  Return next; -- dummy
-}


main_ :: Method
main_ [] this wb k = 
    assign next (Param 100) $ 
    assign next (Sync go [next]) k

go :: Method
go [current] this wb k = 
  assign next (Param (current-1)) $ 
  assign zero (Param 0) $ -- constant
  if_ (BNeg (next `BEq` zero))
     (\ k' -> 
             assign r New $ 
             assign g (Async r go [next]) $ 
             await g k')
     skip $ 
  return_ next wb k -- dummy
  

main :: IO ()
main = run' 10000000 main_ the_end

{- new output
Real steps:	0
Total steps:	35153
-}
