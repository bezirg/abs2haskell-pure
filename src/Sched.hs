-- | The global scheduler that schedules COGs (single-objects in our case)
-- We simulate non-determinism by pseudo-randomness
module Sched (
              run, sched, step
             ) where

import Base
import Eval
import PP ()
import qualified Data.Map as M
import Debug.Trace
import qualified Data.Sequence as S

-- | Given a number of iterations and an entrypoint (ABS main-method) , 
-- it executes the program only for that many iterations returning the final heap.
run :: Int -> Method -> Heap
run iters mainMethod = let
    mainObjRef = 0 -- "main" object (0 ref by default)
    mainFutRef = mainObjRef + 1 -- main-method's destiny (1 ref by default)
    initHeap = Heap { objects = M.singleton mainObjRef (M.singleton "__main__" (-123))
                    , futures = M.singleton mainFutRef Nothing
                    , newRef = mainFutRef+1}
    initProcTable = S.singleton $ (mainObjRef, S.singleton $ Proc (mainObjRef
                                                     ,mainFutRef
                                                     -- async call to main method
                                                     ,mainMethod [] mainObjRef Nothing (\ () -> last_main)
                                                     )
                                   )
    in sched iters (initHeap,initProcTable)
    where
      last_main :: Stmt
      last_main = Return "__main__" Nothing (error "call to main: main is a special block")
                      

-- | The sched is just a looper of the step function
sched :: Int                     -- ^ the max iterations to run
      -> (Heap,ProcTable) -- ^ an initial program configuration
      -> Heap                    -- ^ the final heap
sched n (h,pt) | n < 0 = error "iterations must be positive"
               | n == 0 = trace ("reached max steps\nLast ProcTable: " ++ show pt) h
               | otherwise = case step (h,pt) of
                               Nothing -> trace ("finished (empty proctable), " ++ show n ++ "steps left") h
                               Just conf' -> sched (n-1) conf'

-- | The step is the main workforce. Given a current configuration,
-- it picks up a next object to execute in a round-robin fashion,
-- then pops out its first process, 'eval'uates the 1st statement of that process and returns 
-- either a modified next configuration or 'Nothing' to signal that there was no object left to execute (program finished successfully).
-- Note: Favourably, the step should check if there is no _active_ object left (program finished or program deadlocked).
step :: (Heap,ProcTable)  -- ^ current configuration
     -> Maybe (Heap,ProcTable)  -- ^ new configuration
step (h,pt) | S.length pt == 0 = Nothing
            | otherwise = let
  -- (pickedObjIndex, g') = randomR (0, M.size pt - 1) g
  -- (pickedObjRef, pickedObjProcs) = M.elemAt pickedObjIndex pt
  ((pickedObjRef, pickedObjProcs) S.:< restCogs) = S.viewl pt
  (firstProc@(Proc (this,destiny,_)) S.:< restProcs) =  S.viewl pickedObjProcs
  (execedStmt, mThisCont, mExtraProc, h') = eval firstProc h
  pt' = case mThisCont of
          Nothing -> if S.null restProcs
                    then restCogs -- delete proc-entry
                    else restCogs S.|> (pickedObjRef, restProcs) -- put back the rest
          Just cont' -> restCogs S.|> (pickedObjRef,
                         case execedStmt of
                          Await attr _ -> case M.lookup (readAttr attr this h') (futures $ h') of
                                           -- unresolved
                                           Just Nothing -> restProcs S.|> Proc (this,destiny,cont') -- put it in the back
                                           -- resolved
                                           Just _ -> Proc (this,destiny,cont') S.<| restProcs -- put it in the front
                                           -- this should not happen
                                           Nothing -> error "this should not happen: future not found"
                          _ -> Proc (this,destiny,cont') S.<| restProcs -- otherwise, put it in the front
                        ) 
  pt'' = case mExtraProc of
           Nothing -> pt'
           Just extraProc@(Proc (obj,_,_)) -> case S.findIndexL (\ (obj', _) -> obj == obj') pt' of
                                               Nothing -> pt' S.|> (obj, S.singleton extraProc)
                                               Just i -> S.adjust (\ (objref, procs) -> (objref, procs S.|> extraProc)) i pt'
  in Just (h', pt'')
