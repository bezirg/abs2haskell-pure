module Base where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State (State)
import qualified Data.Map as M (Map)

data Heap = Heap ObjHeap FutHeap -- newtype to remove type-synonym cycle
          deriving Show

type FutHeap = M.Map FutRef (Maybe Ref)
type ObjHeap = M.Map ObjRef Attrs
type Attrs = M.Map String Ref -- attr is a string, this is a mapping from string to vals

type S = State (Counter, -- increasing counter to generate unique new pointer-references from
                Heap)    -- heap of objects vals and futures vals with references

type Counter = Int              -- a counter to pick unique references for newly-created objects and references

type Ref = Int                  -- our values are refs, ObjRefs or FutRefs, assume well-typed
type FutRef = Ref
type ObjRef = Ref

type Process = (Action S, -- action/continuation
                FutRef)             -- returns-to-future

-- our program's actions (effects) are parameterized by m (the underlying monad where actions are ran in).
-- this is a recursive type, because when actions are ran, they return some new action.
data Action m = Atom 
                   (m (Action m)) -- the simplest action that lifts an action of the underlying monad.
              | Async 
                   ObjRef       -- the callee
                   (Action m)   -- the method (function that contains action(s)) to call
                   (Action m)   -- caller continues with this action after the call
                   FutRef       -- the created future 
              | Await 
                   FutRef       -- on which future
                   (Action m)   -- how to continue
              | Get 
                   FutRef       -- on which future
                   (m (Action m)) -- where to store it
              | Return_ Ref          -- return this value
              | Stop              -- empty (finished) process (no continuation included)

-- the ContT monad transformer
newtype C m a = C {unC :: (a -> Action m) -> Action m }

-- cont must be a monad
instance Monad m => Monad (C m) where
    (C f) >>= k = C $ \c -> f (\a -> (unC (k a)) c)
    return x = C $ \c -> c x

-- is a monad transformer my lifting it
instance MonadTrans C where
    lift = atom

type CS = C S

-- Prims
atom :: Monad m => m a -> C m a
atom m = C $ \ c -> Atom (do a <- m ; return (c a))

action :: Monad m => C m a -> Action m
action (C m) = m (\ a -> Stop) -- tying up the knot


