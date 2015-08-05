{-# LANGUAGE ImplicitParams #-}

module Main where

import ABS

next:one:l:r:f:g:the_end:_=[1..]

{-
main := this.go [InitialArgument]
-}

{-

go [current]this :=
  one := 1;                     -- constant
  next := current - 1;
  if (next != one) {
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
    assign next (Param 4) $ 
    assign next (Sync go [next]) k

go :: Method
go [current] this wb k = 
  assign next (Param (current-1)) $ 
  assign one (Param 1) $  -- constant
  if_ (BNeg (next `BEq` one))
     (\ k' -> assign l New $ 
             assign r New $ 
             assign f (Async l go [next]) $ 
             assign g (Async r go [next]) $ 
             await f $ 
             await g k')
     skip $
  return_ next wb k -- dummy
  

main :: IO ()
main = printHeap =<< run 10000000 main_ the_end

{- new output:
Real steps:	0
Total steps:	74
Object Heap with array-size:20{
(9,([(1,1),(2,1)],fromList []))(8,([(1,1),(2,1)],fromList []))(7,([(1,1),(2,1)],fromList []))(6,([(1,1),(2,1)],fromList []))(3,([(1,2),(2,1),(3,7),(4,9),(5,11),(6,13)],fromList []))(2,([(1,2),(2,1),(3,6),(4,8),(5,10),(6,12)],fromList []))(0,([(0,-123),(1,3),(2,1),(3,2),(4,3),(5,4),(6,5)],fromList []))
}
Future Heap with array-size:20{
(13,Right 1)(12,Right 1)(11,Right 1)(10,Right 1)(5,Right 2)(4,Right 2)(1,Right (-123))
}
    Counter: 14
-}
