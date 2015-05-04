module Base where

import Data.Map (Map)

-- Our language's values are Ints (unique references)
type Ref = Int                  -- our values are refs: ObjRefs or FutRefs, assume well-typed
type ObjRef = Ref
type FutRef = Ref
type Counter = Ref              -- a counter to pick unique references for newly-crea

-- | the Heap is just a pair (newtype so we can override its Show instance)
data Heap = Heap { objects :: Objects
                 , futures :: Futures
                 , newRef :: Ref
                 }

type Objects = Map ObjRef Attrs -- object => FieldTable
type Futures = Map FutRef (Maybe Ref)
type Attrs = Map String Ref -- field is a string

-- Continuations 
type Cont = () -> Stmt

-- | a process is a triple of this,destiny,(resumable) continuation
newtype Proc = Proc {fromProc :: (ObjRef, FutRef, Cont)}

-- The scheduler's runtime process table
type ProcTable = Map ObjRef [Proc]

-- | a method is function that takes
type Method = [Ref]             -- ^ a list of passed (deref) parameters
            -> ObjRef            -- ^ this obj
            -> Maybe String      -- ^ in case of sync call: a writeback attribute to write the return result to
            -> Cont              -- ^ the continuation after the method is finished
            -> Cont              -- ^ the created method's continuation that will start executing when applied to ()
-- OUR AST

-- | a single line terminated by ';'
data Stmt = Assign String Rhs Cont
          | Await String Cont
          | If BExp (Cont -> Stmt) (Cont -> Stmt) Cont
          | While BExp (Cont -> Stmt) Cont
          | Skip Cont
          | Return String (Maybe String) Cont
          | Stop

-- | the RHS of assignment
data Rhs = New
         | Get String
         | Async String Method [String]
         | Sync Method [String]
         | Param Ref         -- this lets you put method parameters to fields and use them later
         | Attr String
         -- we do not need This, because it is passed as a local parameter on each method

-- | pointer equality of fields
data BExp = BCon BExp BExp
          | BDis BExp BExp
          | BNeg BExp
          | BEq String String

