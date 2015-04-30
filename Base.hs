module Base where

import qualified Data.Map as M (Map, map)

-- | the Heap is just a pair (newtype so we can override its Show instance)
data Heap = Heap { objects :: Objects
                 , futures :: Futures
                 , newRef :: Ref
                 }

type Objects = M.Map ObjRef Attrs -- object => FieldTable
type Futures = M.Map FutRef (Maybe Ref)
type Attrs = M.Map String Ref -- field is a string

-- Our language's values are Ints (unique references)
type Ref = Int                  -- our values are refs: ObjRefs or FutRefs, assume well-typed
type ObjRef = Ref
type FutRef = Ref
type Counter = Ref              -- a counter to pick unique references for newly-created objects and references


-- | the Program State at any point in time (newtype so we can override its Show instance)
data State = State { ptable :: ProcTable
                   , heap :: Heap
                   , counter :: Counter -- increasing counter to generate unique new pointer-references from
                   }

type ProcTable = M.Map ObjRef [Cont] -- a queue of processes

-- Continuations 
type Cont = Heap -> Pausable

-- Pausable Computations (= execution-step results)
data Pausable = NewP ObjRef Heap Cont 
              | AssignP Heap Cont
              | AwaitP Heap Cont
              | GetP Heap Cont
              | AsyncP (ObjRef, Cont) Heap Cont
              | ReturnP Heap Cont
              | SkipP Cont      -- we do not need the heap
              | StopP

-- | each statement takes as last 2 arguments:
-- 1) the program state
-- 2) and the current (pausable) continuation 
-- returns a pausable computation
type Stmt = Heap             -- the current heap at the point of entry
          -> Cont -- the continuation after the statement is finished
          -> Pausable                   -- the method is a pausable computation

-- | a method is function from a list of passed parameters to a Pausable Computation
type Method = [Ref]             -- the deref-ed args
            -> ObjRef            -- the this obj
            -> Either String FutRef  -- left means sync call and has to write back to that attr, right means async call and respond to that destiny future
            -> Stmt


-- | the RHS of assignment
data Rhs = New
         | Get String
         | Async String Method [String]
         | Sync Method [String]
         | Param Ref         -- this lets you put method parameters to fields and use them later
         | Attr String
           -- we do not need This, because we treat it a predefined string-field "this"

-- | pointer equality of fields
data BExp = BCon BExp BExp
          | BDis BExp BExp
          | BNeg BExp
          | BEq String String

-- for Debugging
----------------

instance Show Heap where
    show (Heap ot ft c) = "ObjTable:" ++ show ot ++ "\n" 
                        ++ "FutTable:"++ show ft
                        ++ "Counter:" ++ show c

instance Show State where
    show (State pt h c) = "Counter:" ++ show c ++ "\n" 
                       -- because we cannot print continuations, we print the length of the queue inside each object
                       ++ "ProcessTable:" ++ show (M.map (\ ps -> length ps) pt) 
                       ++ show h
