module Prim where

import Base
import qualified Data.Vector.Mutable as V
import Control.Monad (when,liftM, liftM2)
import qualified Data.Sequence as S
import Debug.Trace

attrArrSize = 100

skip :: Cont -> Cont
skip k (this,h) = do
  updateFront (this,h) k
  return ([this], h)

if_ :: BExp -> (t -> Cont) -> (t -> Cont) -> t -> Cont
if_ bexp t e k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  bres <- beval bexp attrs
  updateFront (this,h) $ (if bres
                         then t k
                         else e k)
  return ([this],h)


while :: BExp -> (Cont -> Cont) -> Cont -> Cont
while bexp s k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  bres <- beval bexp attrs
  updateFront (this,h) (if bres
                        then s (while bexp s k)
                        else k)
  return ([this], h)

-- NOTE-TO-SELF: updateFront before growing the array

assign lhs New k (this,h) = do
  updateFront (this,h) k
  (attrs,_) <- objects h `V.read` this
  (attrs `V.write` lhs) $ newRef h
  initAttrVec <- V.replicate attrArrSize (-1)
  (objects h `V.write` newRef h) (initAttrVec, S.empty)
  h' <- incCounterMaybeGrow h
  return ([this], h')

assign lhs (Param r) k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  (attrs `V.write` lhs) r
  updateFront (this,h) k
  return ([this], h)

assign lhs (Attr a) k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  (attrs `V.write` lhs) =<< (attrs `V.read` a) 
  updateFront (this,h) k
  return ([this], h)

assign lhs (Get a) k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  f <- attrs `V.read` a
  fval <- (futures h) `V.read` f
  case fval of
    -- unresolved future
    Left blockedCallers -> do
                          when (null blockedCallers) $ (futures h `V.write` f) (Left [this]) -- add this to the blockers
                        --return (res, 
                          -- return -- (GetBlocked, -- dummy instruction to express that 'get' is blocked
                          updateFront (this,h) $ (assign lhs (Get a) k) -- when&if re-scheduled, it should be resolved by re-running
                          return ([], h) -- don't re-sched this right now
    -- already-resolved future
    Right v -> do
              (attrs `V.write` lhs) v 
              updateFront (this,h) k
              return ([this], h)

assign lhs (Sync m params) k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  derefed_params <- mapM (attrs `V.read`) params -- read the passed attrs
  updateFront (this,h) $ (m 
                          derefed_params
                          this
                          (Just lhs)
                          k)
  return ([this], h) 

assign lhs (Async obj m params) k (this,h) = do
  updateFront (this,h) k
  (attrs,_) <- objects h `V.read` this
  calleeObj <- attrs `V.read` obj -- read the callee object
  (calleeAttrs, calleeProcQueue) <- (objects h `V.read` calleeObj)
  derefed_params <- mapM (attrs `V.read`) params -- read the passed attrs
  let newCont = m 
                derefed_params
                calleeObj
                Nothing -- no writeback
                (error "this async method did not call return") -- tying up the knot: nothing left to execute after the process is finished
  let newProc = Proc (newRef h, newCont)
  (attrs `V.write` lhs) (newRef h) 
  (objects h `V.write` calleeObj) (calleeAttrs, calleeProcQueue S.|> newProc)
  (futures h `V.write` newRef h) (Left [])  -- create a new unresolved future
  h' <- incCounterMaybeGrow h
  return (this:[calleeObj  | S.null calleeProcQueue] 
         ,h')

await :: Int -> Cont -> Cont
await attr k (this,h) = do
  (attrs,_) <- objects h `V.read` this
  fut <- V.read (futures h) =<< (attrs `V.read` attr)
  case fut of
    -- unresolved future
    Left _ -> updateBack (this,h) (await attr k) -- loop with await remaining
    -- already-resolved future
    Right _ -> updateFront (this,h) k
  return ([this], h)

return_ :: Int -> Maybe Int -> Cont -> Cont
return_ attr wb k (this,h) = do
  (attrs,pqueue) <- objects h `V.read` this
  case wb of
    -- sync call
    Just lhs -> do
            (attrs `V.write` lhs) =<< (attrs `V.read` attr) 
            updateFront (this,h) k
            return ([this], h)
    -- async call
    Nothing -> case S.viewl pqueue of
                S.EmptyL -> error "this should not happen: running process-queue object"
                (Proc (destiny, _oldProc) S.:< restProcs) -> do
                    fut <- futures h `V.read` destiny
                    case fut of
                      Right _ -> error "this should not happen: tried to return to an already resolved future"
                      Left blockedCallers -> do
                        (futures h `V.write` destiny) =<< liftM Right (attrs `V.read` attr) -- resolve the future
                        (objects h `V.write` this) (attrs, restProcs) -- throws away the current process because it is done
                        return ([this | not $ S.null restProcs] ++ blockedCallers, -- schedule self if there are more processes, and wake-up the blocked callers
                                h)

-- | updates the object's process-queue by pushing to the front the new process (continuation) (or to the back if it resulted from an await)
-- updateObj :: Heap -> Either Cont Cont -> IO ()
updateQueue :: 
              Bool              -- put in front?
            -> (Int,Heap)               
            -> Cont 
            -> IO ()
updateQueue frontp (this,h) k = do
    (attrs, pqueue) <- objects h `V.read` this
    case S.viewl pqueue of
     S.EmptyL -> error "this should not happen: running process-queu object"
     (Proc (destiny, _oldProc) S.:< restProcs) -> (objects h `V.write` this) (attrs, if frontp
                                                                                    then Proc (destiny, k) S.<| restProcs
                                                                                    else restProcs S.|> Proc (destiny, k))
updateFront :: (Int, Heap) -> Cont -> IO ()
updateFront = updateQueue True

updateBack :: (Int, Heap) -> Cont -> IO ()
updateBack = updateQueue False


-- | increases the memory counter and maybe grows (doubles) the heap if it reaches its limit
incCounterMaybeGrow :: Heap -> IO Heap
incCounterMaybeGrow h =  let curSize = V.length (objects h)
                         in
                                 if newRef h + 1 == curSize
                                 then do
                                   objects' <- V.grow (objects h) curSize -- new object heap will have double the current size
                                   futures' <- V.grow (futures h) curSize -- new future heap will have double the current size
                                   return h { objects = objects', 
                                              futures = futures',
                                              newRef = newRef h + 1
                                            }
                                 else return $ h {newRef = newRef h + 1}

-- | Evaluates a predicate BExp from the AST to a Haskell's Bool
beval (BCon exp1 exp2) attrs = liftM2 (&&) (beval exp1 attrs) (beval exp2 attrs)
beval (BDis exp1 exp2) attrs = liftM2 (||) (beval exp1 attrs) (beval exp2 attrs)
beval (BNeg exp1) attrs = liftM not $ beval exp1 attrs
beval (BEq attr1 attr2) attrs = liftM2 (==) (attrs `V.read` attr1) (attrs `V.read` attr2)
