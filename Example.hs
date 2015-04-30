module Example where

import Base

-- EXAMPLE
m1 :: Method
m1 [arg1,arg2,arg3] this wb k = (Await "x") $ 
                            \ () -> If (BEq "x" "x") Skip Skip $
                                  \ () -> Return "x" wb $ k
