module Eval where

import Base
import qualified Data.Map as M

eval :: Proc
     -> Heap
     -> (--TODO: return also the executed stmt Stmt
        Maybe Cont, Maybe Proc, Heap)
eval (this, destiny, c) h = case c () of
  Stop -> (Nothing, Nothing, h)
  Skip k' -> (Just k', Nothing, h)
  Return attr wb k' -> case wb of
                        -- sync call
                        Just lhs -> (Just k'
                                   ,Nothing
                                   , let 
                                        attrEntry = M.singleton lhs (readAttr attr this h)
                                        objects' = M.insertWith M.union this attrEntry (objects h)
                                     in h {objects = objects'})
                        Nothing -> (Nothing
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
                   then (Just $ \ () -> t k', Nothing, h)
                   else (Just $ \ () -> e k', Nothing, h)
  While bexp s k' -> if beval bexp this h
                    then (Just $ \ () -> s (\ () -> While bexp s k'), Nothing, h)
                    else (Just k', Nothing, h)
  Await attr k' -> case M.lookup (readAttr attr this h) (futures h) of
                    Nothing -> error $ "no such future " ++ attr
                    -- unresolved future
                    Just Nothing -> (Just $ \ () -> Await attr k', Nothing, h) -- loop with await remaining
                    -- already-resolved future
                    Just _ -> (Just k', Nothing, h) -- continue without the await stmt
  Assign lhs New k' -> let 
      objects' = M.insert (newRef h) M.empty (objects h)
      objects'' = M.insertWith M.union this attrEntry objects'
      attrEntry = M.singleton lhs (newRef h)
      in (Just k'
         ,Nothing
         ,h { objects = objects''
            , newRef = newRef h + 1})
  Assign lhs (Get f) k' -> case M.lookup (readAttr f this h) (futures h) of
                            Nothing -> error $ "no such future " ++ f
                            -- unresolved future
                            Just Nothing -> (Just $ \ () -> Assign lhs (Get f) k'
                                           ,Nothing
                                           ,h) -- loop with get remaining
                            -- already-resolved future
                            Just (Just v) -> let -- it's the same as -- Assign lhs (Param v) h k
                                         objects' = M.insertWith M.union this attrEntry (objects h)
                                         attrEntry = M.singleton lhs v
                                         in (Just k', Nothing, h { objects = objects' })
  Assign lhs (Sync m attrs) k' -> (Just $ (\ () ->
                                              m 
                                              (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                                              this
                                              (Just lhs)
                                              k')
                                 ,Nothing
                                 ,h)
  Assign lhs (Async obj m attrs) k' ->
      let newCont = \ () -> 
                    m 
                    (map (\ a -> readAttr a this h) attrs) -- read the passed attrs
                    (readAttr obj this h) -- read the callee object
                    Nothing -- no writeback
                    (\ _ -> Stop) -- tying up the knot: nothing left to execute after the process is finished
          newProc = (readAttr obj this h, newRef h, newCont)
          futures' = M.insert (newRef h) Nothing (futures h) -- create a new unresolved future
          attrEntry = M.singleton lhs (newRef h)
          objects' = M.insertWith M.union this attrEntry (objects h)
      in (Just k'
         ,Just newProc
         ,h { objects = objects'
            , futures = futures'
            , newRef = newRef h + 1})
  Assign lhs (Param r) k' -> let 
      objects' = M.insertWith M.union this attrEntry (objects h)
      attrEntry = M.singleton lhs r
      in (Just k', Nothing, h { objects = objects' })
  Assign lhs (Attr a) k' -> let 
      objects' = M.insertWith M.union this attrEntry (objects h)
      attrEntry = M.singleton lhs (readAttr a this h)
      in (Just k', Nothing, h { objects = objects' })
                                                                          
beval :: BExp -> ObjRef -> Heap -> Bool
beval (BCon exp1 exp2) this s = beval exp1 this s && beval exp2 this s
beval (BDis exp1 exp2) this s = beval exp1 this s || beval exp2 this s
beval (BNeg exp1) this s = not (beval exp1 this s)
beval (BEq attr1 attr2) this s = readAttr attr1 this s == readAttr attr2 this s

-- | Utility function
readAttr :: String
         -> ObjRef               -- this
         -> Heap
         -> Ref
readAttr attr this h = case M.lookup this (objects h) of
                    Nothing -> error "\"this\" was not found: developer error"
                    Just as -> case M.lookup attr as of
                                Nothing -> error ("this = " ++ show this ++ ", attr = " ++ attr ++ " not found")
                                Just v -> v
