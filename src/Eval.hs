-- | Contains the single-step evaluator/interpreter of ABS terms (from AST)
module Eval where

import Base
import qualified Data.Sequence as S
import qualified Data.Vector.Mutable as V
import Data.Maybe
import Control.Monad (when,liftM, liftM2)

-- | eval takes a process and a heap and executes the 1st only statement of that process.
-- Returns the executed statement, max 2 objects to reschedule, and a new heap
eval :: ObjRef                     -- ^ the object to execute
     -> Heap                       -- ^ inside a heap
     -> IO (Stmt                    -- the 1st only statement of this process that has been fully executed
       ,[ObjRef]                -- the number of objects that have to be appended to the scheduler's queue 
       , Heap)                  -- the new heap after the execution of the stmt
eval this h = do
  (attrs,pqueue) <- objects h `V.read` this
  case S.viewl pqueue of
     S.EmptyL -> error "this should not happen: scheduled an empty-proc object"
     (Proc (destiny, c)) S.:< restProcs -> let res = c ()
                                          in case res of
        Skip k' -> do
                updateObj $ Left k'
                return (res, [this], h)
        Return attr wb k' -> case wb of
                        -- sync call
                        Just lhs -> do
                          (attrs `V.write` lhs) =<< (attrs `V.read` attr) 
                          updateObj $ Left k'
                          return (res,
                                   [this],
                                   h
                                   )
                        -- async call
                        Nothing -> do
                           fut <- futures h `V.read` destiny
                           when (isJust fut) $ error "tried to return to an already resolved future"
                           -- unresolved future
                           (futures h `V.write` destiny) =<< (liftM Just $ attrs `V.read` attr)
                           (objects h `V.write` this) (attrs, restProcs)
                           return (res, 
                                   [this | not $ S.null restProcs],
                                   (h))
        If bexp t e k' -> do
                        bres <- beval bexp
                        updateObj $ Left $ if bres
                                           then \ () -> t k'
                                           else \ () -> e k'
                        return (res, 
                          [this],
                          h)
        While bexp s k' -> do
                        bres <- beval bexp
                        updateObj $ Left $ if bres
                                           then \ () -> While bexp s k'
                                           else k'
                        return (res,
                                [this],
                                h)
        Await attr k' -> do
          fut <- V.read (futures h) =<< (attrs `V.read` attr)
          case fut of
            -- unresolved future
            Nothing -> do
                     updateObj $ Right $ \ () -> Await attr k' -- loop with await remaining
                     return (res, 
                             [this],
                             h) 
            -- already-resolved future
            Just _ -> do
                     updateObj $ Left k'
                     return (res, 
                             [this],
                              h)
        Assign lhs New k' -> do
                        (attrs `V.write` lhs) $ newRef h
                        updateObj $ Left k'
                        initAttrVec <- V.new 100
                        (objects h `V.write` newRef h) (initAttrVec, S.empty) 
                        return (res,
                                [this],
                                h {newRef = newRef h + 1})
        Assign lhs (Get f) k' -> do
          fut <- V.read (futures h) =<< attrs `V.read` f
          case fut of
            -- unresolved future
            Nothing -> do
                     updateObj $ Left $ \ () -> Assign lhs (Get f) k' 
                     return (res, 
                             [this],
                             h)
                     -- already-resolved future
            Just v -> do
                     (attrs `V.write` lhs) v 
                     updateObj $ Left k'
                     return (res,
                             [this],
                             h)
        Assign lhs (Sync m params) k' -> do
                        derefed_params <- mapM (attrs `V.read`) params -- read the passed attrs
                        updateObj $ Left (m 
                                          derefed_params
                                          this
                                          (Just lhs)
                                          k')
                        return (res, 
                                [this],
                                h) 
        Assign lhs (Async obj m params) k' -> do
            calleeObj <- attrs `V.read` obj -- read the callee object
            (calleeAttrs, calleeProcQueue) <- (objects h `V.read` calleeObj)
            derefed_params <- mapM (attrs `V.read`) params -- read the passed attrs
            let newCont = m 
                          derefed_params
                          calleeObj
                          Nothing -- no writeback
                          (\ _ -> error "this async method did not call return") -- tying up the knot: nothing left to execute after the process is finished
            let newProc = Proc (newRef h, newCont)
            (attrs `V.write` lhs) (newRef h) 
            updateObj (Left k')
            (objects h `V.write` calleeObj) (calleeAttrs, calleeProcQueue S.|> newProc)
            (futures h `V.write` newRef h) Nothing  -- create a new unresolved future
            return (res,
                    (if S.null calleeProcQueue then (calleeObj:) else id) [this]
                   ,h {newRef = newRef h + 1})
        Assign lhs (Param r) k' -> do
                        (attrs `V.write` lhs) r
                        updateObj $ Left k'
                        return (res, 
                                [this],
                                h)
        Assign lhs (Attr a) k' -> do
                        (attrs `V.write` lhs) =<< (attrs `V.read` a) 
                        updateObj $ Left k'
                        return (res,
                                [this], 
                                h)
      where
        updateObj :: Either Cont Cont -> IO ()
        updateObj ek = (objects h `V.write` this) (attrs, case ek of
                                                            Left k -> Proc (destiny, k) S.<| restProcs
                                                            Right k -> restProcs S.|> Proc (destiny, k)) 
                                                                          
        -- | Evaluates a predicate BExp from the AST to a Haskell's Bool
        beval :: BExp -> IO Bool
        beval (BCon exp1 exp2) = liftM2 (&&) (beval exp1) (beval exp2)
        beval (BDis exp1 exp2) = liftM2 (||) (beval exp1) (beval exp2)
        beval (BNeg exp1) = liftM not $ beval exp1
        beval (BEq attr1 attr2) = liftM2 (==) (attrs `V.read` attr1) (attrs `V.read` attr2)

        -- -- | Utility function to read an attribute from an object (this)
        -- readAttr :: String               -- ^ the name of the attribute
        --          -> Ref                  -- ^ its value
        -- readAttr attr = case M.lookup attr attrs of
        --                         Nothing -> error ("this = " ++ show this ++ ", attr = " ++ attr ++ " not found")
        --                         Just v -> v
