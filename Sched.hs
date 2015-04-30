module Sched where

import Base
import Eval


sched :: Int -> Heap -> ProcTable -> Heap
sched 0 h pt = h
sched n h pt = (uncurry $ sched (n-1)) (step h proctable))

step :: Heap -> M.Map ObjRef [Proc] -> (Heap, M.Map ObjRef [Proc])
step h proctable = do
  o <- rand (M.keys proctable)
  (p@(this,destiny,_) : rest) <- M.lookup o proctable
  (execstmt, mthiscont, mnewproc, h') <- eval h p 
  case execsstmt
       Get ... -> (this, destiny, c'): rest -- put in the front, or add it to a disabled set
       Assign (New) -> M.insert (counter-1) []
       _ -> fdsfsafsd                       -- round-robin
  case mnewproc@(obj,dest,c)
       Just _ ->  
