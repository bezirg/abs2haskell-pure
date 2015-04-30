module EDSL where

import Base
import qualified Data.Map as M

skip :: Stmt
skip _h k = SkipP k

assign  :: String                -- attr
        -> Rhs                   
        -> Ref                   -- this
        -> Stmt
assign lhs (Param r) this h k = let objects' = M.insertWith M.union this attrEntry (objects h)
                                    attrEntry = M.singleton lhs r
                                in AssignP (h { objects = objects' }) k

assign lhs (Attr a) this h k = let objects' = M.insertWith M.union this attrEntry (objects h)
                                   attrEntry = M.singleton lhs (readAttr a this h)
                               in AssignP (h { objects = objects' }) k

assign lhs New this h k = let objects' = M.insert (newRef h) M.empty (objects h)
                              objects'' = M.insertWith M.union this attrEntry objects'
                              attrEntry = M.singleton lhs (newRef h)
                          in NewP 
                             (newRef h)
                             (h { objects = objects''
                                , newRef = newRef h + 1})
                             k

assign lhs (Get f) this h k = case M.lookup (readAttr f this h) (futures h) of
                                Nothing -> error $ "no such future " ++ f
                                -- unresolved future
                                Just Nothing -> GetP h (\ h' -> assign lhs (Get f) this h' k) -- loop with get remaining
                                -- already-resolved future
                                Just (Just v) -> assign lhs (Param v) this h k -- treat it the same as passed value

-- o!m([attrs])
assign lhs (Async obj m attrs) this h k = -- let ptable' = M.insertWith (++) this [newProc] (ptable s)
                                          --     -- here we construct a new continuation = new process
                                          let newProc = \ h' -> -- the new proc will take not the current heap, but a future heap
                                                        m 
                                                        (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                                                        (readAttr obj this h) -- read the callee object
                                                        (Right $ newRef h) -- the destiny futureref
                                                        h' -- the future heap when the process will start
                                                        (\ _ -> StopP) -- tying up the knot: nothing left to execute after the process is finished
                                              futures' = M.insert (newRef h) Nothing (futures h) -- create a new unresolved future
                                              attrEntry = M.singleton lhs (newRef h)
                                              objects' = M.insertWith M.union this attrEntry (objects h)
                                          in AsyncP
                                             (newRef h, newProc)
                                             (h { objects = objects'
                                                , futures = futures'
                                                , newRef = newRef h + 1})
                                             k
-- this.m([attrs])
assign lhs (Sync m attrs) this h k = AssignP
                                     h
                                     (\ h' ->
                                     m 
                                     (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                                     this -- always sync to this object
                                     (Left lhs) -- where to writeback the result of the sync call
                                     h'       -- jump immediately with the current state
                                     k) -- after the method call finishes, continue with our cc

return :: String                 -- the attr to return
       -> ObjRef                 -- this
       -> Either String FutRef           -- maybe a destiny
       -> Stmt
return attr this destiny h k = case destiny of
                                 -- originates from sync call
                                 Left lhs -> let 
                                                attrEntry = M.singleton lhs (readAttr attr this h)
                                                objects' = M.insertWith M.union this attrEntry (objects h)
                                            in ReturnP (h {objects = objects'}) k
                                 -- originates from async call
                                 Right f -> case M.lookup f (futures h) of
                                       -- unresolved future
                                       Just Nothing -> let futures' = M.insert f (Just $ readAttr attr this h) (futures h)
                                                      in ReturnP (h { futures = futures' }) k
                                       -- already-resolved future
                                       Just (Just _) -> error "tried to return to an already resolved future"
                                       -- null future
                                       Nothing -> error "future not in the heap"

await :: String
      -> ObjRef                  -- this
      -> Stmt
await f this h k = AwaitP h $ case M.lookup (readAttr f this h) (futures h) of
                                Nothing -> error $ "no such future " ++ f
                                -- unresolved future
                                Just Nothing -> (\ h' -> await f this h' k) -- loop with await remaining
                                -- already-resolved future
                                Just _ -> k -- continue without the await stmt

readAttr :: String
         -> ObjRef               -- this
         -> Heap
         -> Ref
readAttr attr this h = case M.lookup this (objects h) of
                    Nothing -> error "\"this\" was not found: developer error"
                    Just as -> case M.lookup attr as of
                                Nothing -> error ("this = " ++ show this ++ ", attr = " ++ attr ++ " not found")
                                Just v -> v

-- If & While
ifthenelse :: BExp               -- predicate
           -> Stmt           -- then branch
           -> Stmt           -- else branch
           -> ObjRef             -- this
           -> Stmt
ifthenelse bexp stmt1 stmt2 this s k = SkipP $ if beval bexp this s
                                               then (\ h' -> stmt1 h' k)
                                               else (\ h' -> stmt2 h' k)
                              
while :: BExp
      -> Stmt
      -> ObjRef
      -> Stmt
while bexp stmt this h k = SkipP $ if beval bexp this h
                                   then
                                       (\ h' -> stmt h' (\ h'' -> while bexp stmt this h'' k)) -- when resumed, it loops
                                   else k


beval :: BExp -> ObjRef -> Heap -> Bool
beval (BCon exp1 exp2) this s = beval exp1 this s && beval exp2 this s
beval (BDis exp1 exp2) this s = beval exp1 this s || beval exp2 this s
beval (BNeg exp1) this s = not (beval exp1 this s)
beval (BEq attr1 attr2) this s = readAttr attr1 this s == readAttr attr2 this s

-- Shorthands (so all DSL constructs look to be lower-case)
new = New
get = Get
param = Param
field = Attr
(&&:) = BCon
(||:) = BDis
(!:) = BNeg
(==:) = BEq

