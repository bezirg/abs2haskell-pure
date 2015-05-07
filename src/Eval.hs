-- | Contains the single-step evaluator/interpreter of ABS terms (from AST)
module Eval where

import Base
import qualified Data.Map as M

-- | eval takes a process and a heap and executes the 1st only statement of that process.
-- Returns the executed statement, maybe the rest continuation of this process, maybe a brand-new process (from async) and a new heap.
eval :: Proc                     -- ^ a process to execute
     -> Heap                     -- ^ inside a heap
     -> (Stmt                    -- the 1st only statement of this process that has been fully executed
       ,Maybe Cont              -- maybe a continuation (after the stmt) of the same process to be put back to the proctable
       ,Maybe Proc              -- maybe a brand-new extra process (resulting from an async call)
       , Heap)                  -- the new heap after the execution of the stmt
eval (Proc (this, destiny, c)) h = let res = c ()
                            in case res of
  Skip k' -> (res, Just k', Nothing, h)
  Return attr wb k' -> case wb of
                        -- sync call
                        Just lhs -> (res, Just k'
                                   ,Nothing
                                   , let 
                                        attrEntry = M.singleton lhs (readAttr attr this h)
                                        objects' = M.insertWith M.union this attrEntry (objects h)
                                     in h {objects = objects'})
                        -- async call
                        Nothing -> (res, 
                                   Nothing
                                  ,Nothing
                                  , case M.lookup destiny (futures h) of
                                      -- unresolved future
                                      Just Nothing -> let futures' = M.insert destiny
                                                                      (Just $ readAttr attr this h) (futures h)
                                                     in (h { futures = futures' })
                                      -- already-resolved future
                                      Just (Just _) -> error "tried to return to an already resolved future"
                                      -- null future
                                      Nothing -> error "future not in the heap")
  If bexp t e k' -> if beval bexp this h
                   then (res, Just $ \ () -> t k', Nothing, h)
                   else (res, Just $ \ () -> e k', Nothing, h)
  While bexp s k' -> if beval bexp this h
                    then (res, Just $ \ () -> s (\ () -> While bexp s k'), Nothing, h)
                    else (res, Just k', Nothing, h)
  Await attr k' -> case M.lookup (readAttr attr this h) (futures h) of
                    Nothing -> error $ "no such future " ++ attr
                    -- unresolved future
                    Just Nothing -> (res, Just $ \ () -> Await attr k', Nothing, h) -- loop with await remaining
                    -- already-resolved future
                    Just _ -> (res, Just k', Nothing, h) -- continue without the await stmt
  Assign lhs New k' -> let 
      objects' = M.insert (newRef h) M.empty (objects h)
      objects'' = M.insertWith M.union this attrEntry objects'
      attrEntry = M.singleton lhs (newRef h)
      in (res, Just k'
         ,Nothing
         ,h { objects = objects''
            , newRef = newRef h + 1})
  Assign lhs (Get f) k' -> case M.lookup (readAttr f this h) (futures h) of
                            Nothing -> error $ "no such future " ++ f
                            -- unresolved future
                            Just Nothing -> (res, Just $ \ () -> Assign lhs (Get f) k'
                                           ,Nothing
                                           ,h) -- loop with get remaining
                            -- already-resolved future
                            Just (Just v) -> let -- it's the same as -- Assign lhs (Param v) h k
                                         objects' = M.insertWith M.union this attrEntry (objects h)
                                         attrEntry = M.singleton lhs v
                                         in (res, Just k', Nothing, h { objects = objects' })
  Assign lhs (Sync m attrs) k' -> (res, Just $ (m 
                                          (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                                          this
                                          (Just lhs)
                                          k')
                                 ,Nothing
                                 ,h)
  Assign lhs (Async obj m attrs) k' ->
      let newCont = m 
                    (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                    (readAttr obj this h) -- read the callee object
                    Nothing -- no writeback
                    (\ _ -> error "this async method did not call return") -- tying up the knot: nothing left to execute after the process is finished
          newProc = Proc (readAttr obj this h, newRef h, newCont)
          futures' = M.insert (newRef h) Nothing (futures h) -- create a new unresolved future
          attrEntry = M.singleton lhs (newRef h)
          objects' = M.insertWith M.union this attrEntry (objects h)
      in (res, Just k'
         ,Just newProc
         ,h { objects = objects'
            , futures = futures'
            , newRef = newRef h + 1})
  Assign lhs (Param r) k' -> let 
      objects' = M.insertWith M.union this attrEntry (objects h)
      attrEntry = M.singleton lhs r
      in (res, Just k', Nothing, h { objects = objects' })
  Assign lhs (Attr a) k' -> let 
      objects' = M.insertWith M.union this attrEntry (objects h)
      attrEntry = M.singleton lhs (readAttr a this h)
      in (res, Just k', Nothing, h { objects = objects' })
                                                                          
-- | Evaluates a predicate BExp from the AST to a Haskell's Bool
beval :: BExp -> ObjRef -> Heap -> Bool
beval (BCon exp1 exp2) this s = beval exp1 this s && beval exp2 this s
beval (BDis exp1 exp2) this s = beval exp1 this s || beval exp2 this s
beval (BNeg exp1) this s = not (beval exp1 this s)
beval (BEq attr1 attr2) this s = readAttr attr1 this s == readAttr attr2 this s

-- | Utility function to read an attribute from an object (this)
readAttr :: String               -- ^ the name of the attribute
         -> ObjRef               -- ^ the this object
         -> Heap                 -- ^ the current heap
         -> Ref                  -- ^ its value
readAttr attr this h = case M.lookup this (objects h) of
                    Nothing -> error "\"this\" was not found: developer error"
                    Just as -> case M.lookup attr as of
                                Nothing -> error ("this = " ++ show this ++ ", attr = " ++ attr ++ " not found")
                                Just v -> v
