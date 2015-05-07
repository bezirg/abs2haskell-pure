-- | Contains the single-step evaluator/interpreter of ABS terms (from AST)
module Eval where

import Base
import qualified Data.Sequence as S
import qualified Data.Map as M

-- | eval takes a process and a heap and executes the 1st only statement of that process.
-- Returns the executed statement, max 2 objects to reschedule, and a new heap
eval :: ObjRef                     -- ^ the object to execute
     -> Heap                       -- ^ inside a heap
     -> (Stmt                    -- the 1st only statement of this process that has been fully executed
       ,[ObjRef]                -- the number of objects that have to be appended to the scheduler's queue 
       , Heap)                  -- the new heap after the execution of the stmt
eval this h = case M.lookup this $ objects h of
  Nothing -> error "this should not happen: object not found"
  Just (attrs, pqueue) -> case S.viewl pqueue of
     S.EmptyL -> error "this should not happen: scheduled an empty-proc object"
     (Proc (destiny, c)) S.:< restProcs -> let res = c ()
                                                in case res of
        Skip k' -> (res, [this], h { objects = updateObj attrs $ Left k'})
        Return attr wb k' -> case wb of
                        -- sync call
                        Just lhs -> (res,
                                   [this],
                                   let 
                                        attrs' = M.insert lhs (readAttr attr) attrs
                                   in h { objects = updateObj attrs' $ Left k'}
                                   )
                        -- async call
                        Nothing -> (res, 
                                   [this | not $ S.null restProcs],
                                   case M.lookup destiny (futures h) of
                                      -- unresolved future
                                      Just Nothing -> let futures' = M.insert destiny
                                                                      (Just $ readAttr attr) (futures h)
                                                     in (h { futures = futures',
                                                             objects = M.insert this (attrs, restProcs) $ objects h
                                                           })
                                      -- already-resolved future
                                      Just (Just _) -> error "tried to return to an already resolved future"
                                      -- null future
                                      Nothing -> error "future not in the heap")
        If bexp t e k' -> (res, 
                          [this],
                          h {objects = updateObj attrs $ Left $ if beval bexp
                                                               then \ () -> t k'
                                                               else \ () -> e k'})
        While bexp s k' -> (res,
                          [this],
                           h {objects = updateObj attrs $ Left $ if beval bexp
                                                                 then \ () -> While bexp s k'
                                                                 else k'})
        Await attr k' -> case M.lookup (readAttr attr) (futures h) of
                    Nothing -> error $ "no such future " ++ attr
                    -- unresolved future
                    Just Nothing -> (res, 
                                    [this],
                                    h { objects = updateObj attrs $ Right $ \ () -> Await attr k'}) -- loop with await remaining
                    -- already-resolved future
                    Just _ -> (res, 
                              [this],
                              h { objects = updateObj attrs $ Left k'})
        Assign lhs New k' -> let 
            attrs' = M.insert lhs (newRef h) attrs
            objects' = updateObj attrs' $ Left k'
            objects'' = M.insert (newRef h) (M.empty,S.empty) objects'
            in (res,
                [this],
                h { objects = objects''
                  , newRef = newRef h + 1})
        Assign lhs (Get f) k' -> case M.lookup (readAttr f) (futures h) of
                            Nothing -> error $ "no such future " ++ f
                            -- unresolved future
                            Just Nothing -> (res, 
                                            [this],
                                            h { objects = updateObj attrs $ Left $ \ () -> Assign lhs (Get f) k' })
                                            
                            -- already-resolved future
                            Just (Just v) -> let -- it's the same as -- Assign lhs (Param v) h k
                                         attrs' = M.insert lhs v attrs
                                         in (res,
                                             [this],
                                             h { objects = updateObj attrs' $ Left k' })
        Assign lhs (Sync m params) k' -> (res, 
                                        [this],
                                        h { objects = updateObj attrs $ Left (m 
                                          (map readAttr params) -- read the passed attrs
                                          this
                                          (Just lhs)
                                          k')})
        Assign lhs (Async obj m params) k' ->
            let 
                calleeObj = readAttr obj -- read the callee object
                (calleeAttrs, calleeProcQueue) = maybe (error "this should not happen: callee object not found") id $ M.lookup calleeObj (objects h)
                newCont = m 
                    (map readAttr params) -- read the passed attrs
                    calleeObj
                    Nothing -- no writeback
                    (\ _ -> error "this async method did not call return") -- tying up the knot: nothing left to execute after the process is finished
                newProc = Proc (newRef h, newCont)
                futures' = M.insert (newRef h) Nothing (futures h) -- create a new unresolved future
                attrs' = M.insert lhs (newRef h) attrs
                objects' = updateObj attrs' (Left k')
                objects'' = M.insert calleeObj (calleeAttrs, calleeProcQueue S.|> newProc)  objects'
            in (res,
                (if S.null calleeProcQueue then (calleeObj:) else id) [this]
               ,h { objects = objects''
                  , futures = futures'
                  , newRef = newRef h + 1})
        Assign lhs (Param r) k' -> let 
            attrs' = M.insert lhs r attrs
            in (res, 
                [this],
                h { objects = updateObj attrs' (Left k')})
        Assign lhs (Attr a) k' -> let 
            attrs' = M.insert lhs (readAttr a) attrs
            in (res,
                [this], 
                h { objects = updateObj attrs' (Left k')})
      where
        updateObj :: Attrs -> Either Cont Cont -> Objects
        updateObj as' ek = M.insert this (as', case ek of
                                                 Left k -> Proc (destiny, k) S.<| restProcs
                                                 Right k -> restProcs S.|> Proc (destiny, k)) (objects h)
                                                                          
        -- | Evaluates a predicate BExp from the AST to a Haskell's Bool
        beval :: BExp -> Bool
        beval (BCon exp1 exp2) = beval exp1 && beval exp2 
        beval (BDis exp1 exp2) = beval exp1 || beval exp2 
        beval (BNeg exp1) = not (beval exp1)
        beval (BEq attr1 attr2) = readAttr attr1 == readAttr attr2

        -- | Utility function to read an attribute from an object (this)
        readAttr :: String               -- ^ the name of the attribute
                 -> Ref                  -- ^ its value
        readAttr attr = case M.lookup this (objects h) of
                    Nothing -> error "\"this\" was not found: developer error"
                    Just (as,_) -> case M.lookup attr as of
                                Nothing -> error ("this = " ++ show this ++ ", attr = " ++ attr ++ " not found")
                                Just v -> v
