module Base where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State (State)
import qualified Data.Map as M (Map)

data Heap = Heap ObjHeap FutHeap -- newtype to remove type-synonym cycle
          deriving Show

type FutHeap = M.Map FutRef (Maybe Int)
type ObjHeap = M.Map ObjRef Attrs

type SharedState = State (Counter, -- increasing counter to generate unique new pointer-references from
                          Heap)    -- heap of objects vals and futures vals with references

type Counter = Int

type Attrs = M.Map String Ref -- attr is a string, this is a mapping from string to vals

type Ref = Int                  -- our values are refs, ObjRefs or FutRefs, assume well-typed
type FutRef = Int
type ObjRef = Int

type Process = (Action SharedState, -- action/continuation
                FutRef)             -- returns-to-future

data Action m = Atom (m (Action m))
              | Async ObjRef (Action m) (Action m) FutRef
              | Await FutRef (Action m)
              | Get FutRef (m (Action m))
              | Stop Ref
              | Done

-- the ContT monad transformer
newtype C m a = C {unC :: (a -> Action m) -> Action m }

-- cont must be a monad
instance Monad m => Monad (C m) where
    (C f) >>= k = C $ \c -> f (\a -> (unC (k a)) c)
    return x = C $ \c -> c x

-- is a monad transformer my lifting it
instance MonadTrans C where
    lift = atom

type CS = C SharedState

-- Prims
atom :: Monad m => m a -> C m a
atom m = C $ \ c -> Atom (do a <- m ; return (c a))

action :: Monad m => C m a -> Action m
action (C m) = m (\ a -> Done) -- tying up the knot


