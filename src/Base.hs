-- | The ABS' AST and the ABS-runtime data-structures
module Base where

import Data.Sequence (Seq)
import Data.Vector.Mutable (IOVector) 

-- Initial size of Vectors
inisize :: Int
inisize = 10000

-- * The values of our language

-- | We only have one type for our values: the 'Ref'erence
-- Both objects and futures use this type 'Ref'.
-- Since in this ABS-subset we are dynamically-typed, it is undefined what happens
-- if an object is expected in place of a future, and vice-versa.
type Ref = Int                  -- our values are refs: ObjRefs or FutRefs, assume well-typed

-- | Objects have type 'Ref'
type ObjRef = Ref

-- | Futures have type 'Ref'
type FutRef = Ref

-- | An ever-increasing counter to pick new unique references when
-- creating new objects (via new) and futures (via async)
-- (written as type-synonym just for clarity)
type Counter = Ref              -- a counter to pick unique references for newly-crea

-- * The datastructures used at ABS-runtime

-- | the simulated Heap datastructure where all program's objects live in.
-- We assume no (automatic) Garbage-Collection of the Heap for the moment. In reality,
-- no GC will be performed from the underlying Haskell-GC, because we do not delete objects 
data Heap = Heap { objects :: Objects -- ^ the live objects
                 , futures :: Futures -- ^ the live futures
                 , newRef :: Counter  -- ^ the counter
                 }

-- | The objects of the heap is a _growable_ int-indexed array where each cell is a pair of the object's attributes and the object's process queue
type Objects = IOVector (Attrs, Seq Proc)

-- | The attributes is a __fixed__ array of the attributes values ('Ref's)
--
-- We assume some static analysis to collect the possible attributes and transform them to Ints so they can become the indices to this array.
type Attrs = IOVector Ref

-- | The futures of the heap is a _growable_ int-indexed array of the futures' potential final values.
-- 
-- A future is empty ('Left'), with n callers waiting on it (for our case it is only 1 caller waiting) 
-- A future is resolved (filled with 'Right value'), hence its type "Either [ObjRef] Ref".
-- A future reference must be 'final' after resolved, and this must be guaranteed by the runtime system.
type Futures = IOVector (Either [ObjRef] Ref)

-- | We have a single (universal) type for our continuations. 
-- Later, if we introduce local-variables we are going to need an extra type for Continuations: 'Ref -> Stmt'
type Cont = () -> Stmt

-- | Each process is a pair of its destiny, and its (resumable) continuation
--
-- (note: is a newtype just for overriding its Show instance, check module "PP")
newtype Proc = Proc {fromProc :: (FutRef, Cont)}

-- | The (global) scheduler's runtime Process Table.
--
-- It is a round-robin queue _from_ an object-reference _to_ a double-ended queue of processes beloning to that object
type SchedQueue = Seq ObjRef

-- * Our language's AST and types

-- | An ABS statement.
--
-- Statements are "chained" (sequantially composed) by deeply nesting them through 'Cont'inuations.
data Stmt = Assign Int Rhs Cont -- ^ "attr" := Rhs; cont...
          | Await Int Cont      -- ^ await "attr"; cont...
          | If BExp (Cont -> Stmt) (Cont -> Stmt) Cont -- ^ if pred ThenClause ElseClause; cont... 
          | While BExp (Cont -> Stmt) Cont             -- ^ while pred BodyClause; cont...
          | Skip Cont                                  -- ^ skip; cont...
          | Return Int (Maybe Int) Cont                -- ^ return "attr" WriteBack; cont... (note: if it is a sync call then we pass as an argument to return, the attribute to write back to, if it is async call then we pass Nothing)
          | GetBlocked                                 -- ^ Dummy instruction to be returned by sched'

-- | the RHS of an assignment
data Rhs = New
         | Get Int
         | Async Int Method [Int]
         | Sync Method [Int]
         | Param Ref         -- ^ all commands operate on attributes; to use instead a method's parameter (passed argument or this) you first store it to an (auxiliary) attribute, e.g. Assign "attr" (Param this)
         | Attr Int       -- ^ assign an attribute to the value of another attribute
         -- we do not need This, because it is passed as a local parameter on each method

-- | A boolean expression occurs only as a control-flow predicate (if & while) 
--
-- It does only reference equality of attributes ('BEq') and combinators on them (conjuction,disjunction,negation).
data BExp = BEq Int Int
          | BNeg BExp
          | BCon BExp BExp
          | BDis BExp BExp



-- | The type of every top-level ABS-method.
type Method = [Ref]             -- ^ a list of passed (deref) parameters
            -> ObjRef            -- ^ this obj
            -> Maybe Int         -- ^ in case of sync call: a writeback attribute to write the return result to
            -> Cont              -- ^ the continuation after the method is finished
            -> Cont              -- ^ the resulting method's continuation that will start executing when applied to ()


