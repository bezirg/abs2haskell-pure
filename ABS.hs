module ABS where

import Base
import Control.Monad (liftM, liftM2)
import qualified Control.Monad.Trans.State as S
import qualified Data.Map as M
import Data.List.Utils (addToAL)
import Data.Maybe (fromJust)

-- Statements
-------------
writeAttr :: Ref -> String -> CS Ref -> CS ()
writeAttr this a lv = lv >>= \ v -> atom $ do
  (c, Heap oh fh) <- S.get
  let attrs = fromJust $ M.lookup this oh
  let attrs' =  M.insert a v attrs -- update attrs
  let oh' = M.insert this attrs' oh -- update object in the objectheap
  S.put (c, Heap oh' fh)

async :: CS ObjRef -> (Ref -> CS a) -> CS FutRef
async co1 m = co1 >>= \ o1 -> do
  -- pick a next future
  c <- atom $ do 
        (c, h) <- S.get
        S.put (c+1, h)
        return c
  (C $ \ k -> Async o1 (action (m o1)) (k c) c) -- spawn another process and call this process' continuation passing the new futureref

ifM :: CS Bool -> CS () -> CS () -> CS ()
ifM b t e = do
  r <- b
  if r then t else e

skip :: CS ()
skip = return ()

while :: CS Bool -> CS () -> CS ()
while b stmts = ifM b (stmts >> while b stmts) (return ())

yield :: Monad m => FutRef -> C m ()
yield f = C $ \ c -> Await f (c ())

return_ :: Monad m => C m Ref -> C m Ref -- val is a ref
return_ cv = cv >>= \ v -> C $ \ c -> Return_ v

-- Expressions (left-hand side LHS only)
----------------------------------------
readAttr :: Ref -> String -> CS Ref
readAttr this a = atom $ do
                (_, Heap oh _) <- S.get
                let attrs = maybe (error "debug: typing error, program is not well-typed") id (M.lookup this oh) -- fin: replace with fromJust
                return $ maybe (error ("debug: typing error, no such attr " ++ a ++ " in object:" ++ show this) ) id (M.lookup a attrs) -- fin: replace with fromJust

newObject :: CS ObjRef
newObject = atom $ do
  (c, Heap objh futh) <- S.get  
  S.put $ (c+1, Heap (M.insertWith (const $ const $ error "debug: new:already in the map, impl problem, to be removed") c M.empty objh) futh) -- fin: replace with M.insert
  return c

getFuture :: CS FutRef -> CS Ref
getFuture cf = cf >>= \ f -> C $ \ c -> Get f (do
                                 (_, Heap _ fh) <- S.get
                                 let fv = fromJust $ fromJust $ M.lookup f fh
                                 return (c fv))

-- The scheduler loop
---------------------
sched :: [(ObjRef, [Process])] -> S ()
sched [] = return ()
sched ((_, []): os) = sched os -- an object's queue is done
sched ((o, ((a, rf):ps)):os) = case a of
    Atom am -> do 
      a' <- am;                           -- execute atomic action
      sched $ (o, ((a',rf):ps)):os -- continue with next atomic step of the *same process*
    Async o1 a1 a' f -> sched $ if o1 == o             -- this ! m(..);
                               then os ++ [(o, ps ++ [(a', rf),(a1,f)])]
                               else case lookup o1 os of
                                      Nothing -> os ++ [(o, ps ++ [(a', rf)]), (o1, [(a1,f)])]
                                      Just ps1 -> addToAL os o1 (ps1 ++ [(a1,f)]) ++ [(o, ps ++ [(a', rf)])]
    Await f a' -> do
      v <- S.gets (lookupFuture f)
      case v of
        Nothing -> sched $ os ++ [(o, ps ++ [(a, rf)])] -- future-result not available, move to next process
        Just _ -> sched $ (o, (a', rf):ps):os -- result is available, continue immediately to its rest continuation
    Get f am -> do
      v <- S.gets (lookupFuture f)
      case v of
        Nothing -> sched $ os ++ [(o,(a, rf):ps)] -- move to next process, but block this object on this process only (busy-wait for this object)
        Just _ -> do a' <- am; sched $ (o, (a', rf):ps):os -- result is available, continue immediately to its rest continuation
    Return_ v -> do -- method-process returned
      S.modify (\ (c, Heap oh fh) -> (c, Heap oh (M.insert rf (Just v) fh))) -- update the future in the heap
      sched (os ++ [(o, ps)])   -- continue
    Stop -> if o == 0 
           then sched (os ++ [(o, ps)]) -- main *process* exited, not main object 
           else error "Compilation error, return must be the last statement" -- if this == 0 (main) then ok else error
    where
      lookupFuture :: FutRef -> (t, Heap) -> Maybe (Maybe Int)
      lookupFuture f (_,Heap _ fh) = M.lookup f fh

-- Our Main entry-point
-----------------------
run :: (Ref -> CS ()) -> (Counter, Heap)
run m = S.execState
        (sched [(0, -- main-object has objectref 0
                    -- staring a single main process
                [(action (m 0), error "main does not return")] -- main cannot return
               )])
        (1,Heap (M.singleton 0 M.empty) M.empty) -- initial heap: just the main object and no attrs

-- Lifted boolean operators
(===) :: Eq a => CS a -> CS a -> CS Bool
(===) = liftM2 (==)
(&&&) :: CS Bool -> CS Bool -> CS Bool
(&&&) = liftM2 (&&)
(|||) :: CS Bool -> CS Bool -> CS Bool
(|||) = liftM2 (||)
notM :: CS Bool -> CS Bool
notM = liftM (not)

