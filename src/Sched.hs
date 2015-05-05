-- | The global scheduler that schedules COGs (single-objects in our case)
-- We simulate non-determinism by pseudo-randomness
module Sched (
              run, runFixed, runIO, sched, step
             ) where

import Base
import Eval
import PP ()
import System.Random
import qualified Data.Map as M
import Debug.Trace

-- | Wrapper to 'run' using a "true" stdgen from the hardware clock
runIO :: Int -> Method -> IO Heap
runIO n mainMethod = do
  g <- getStdGen
  return (run n mainMethod g)

-- | Wrapper to 'run' using a default (fixed) stdgen
runFixed :: Int -> Method -> Heap
runFixed n mainMethod = run n mainMethod (mkStdGen maxBound)

-- | Given a number of iterations and an entrypoint (ABS main-method) , 
-- it executes the program only for that many iterations returning the final heap.
run :: Int -> Method -> StdGen -> Heap
run iters mainMethod g = let
    mainObjRef = 0 -- "main" object (0 ref by default)
    mainFutRef = mainObjRef + 1 -- main-method's destiny (1 ref by default)
    initHeap = Heap { objects = M.singleton mainObjRef M.empty 
                    , futures = M.singleton mainFutRef Nothing
                    , newRef = mainFutRef+1}
    initProcTable = M.singleton mainObjRef [Proc (mainObjRef
                                            ,mainFutRef
                                            -- async call to main method
                                            ,mainMethod [] mainObjRef Nothing (\ () -> Stop)
                                            )]
    in sched iters (initHeap,initProcTable,g)
                      

-- | The sched is just a looper of the step function
sched :: Int                     -- ^ the max iterations to run
      -> (Heap,ProcTable,StdGen) -- ^ an initial program configuration
      -> Heap                    -- ^ the final heap
sched n (h,pt,g) | n < 0 = error "iterations must be positive"
                 | n == 0 = trace ("reached max steps\nLast ProcTable: " ++ show pt) h
                 | otherwise = case step (h,pt,g) of
                                 Nothing -> trace ("finished (empty proctable), " ++ show n ++ "steps left") h
                                 Just conf' -> sched (n-1) conf'

-- | The step is the main workforce. Given a current configuration,
-- it randomly picks up a next object to execute,
-- then pops out its first process, 'eval'uates the 1st statement of that process and returns 
-- either a modified next configuration or 'Nothing' to signal that there was no object left to execute (program finished successfully).
-- Note: Favourably, the step should check if there is no _active_ object left (program finished or program deadlocked).
step :: (Heap,ProcTable,StdGen)  -- ^ current configuration
     -> Maybe (Heap,ProcTable,StdGen)  -- ^ new configuration
step (h,pt,g) | M.size pt == 0 = Nothing
              | otherwise = let
  (pickedObjIndex, g') = randomR (0, M.size pt - 1) g
  (pickedObjRef, pickedObjProcs) = M.elemAt pickedObjIndex pt
  (firstProc@(Proc (this,destiny,_)) : restProcs) =  pickedObjProcs
  (execedStmt, mThisCont, mExtraProc, h') = eval firstProc h
  pt' = case mThisCont of
          Nothing -> if null restProcs
                    then M.delete this pt -- delete proc-entry
                    else M.insert this restProcs pt -- put back the rest
          Just cont' -> M.insert 
                       this 
                       (case execedStmt of
                          Assign _ (Get _) _ -> Proc (this, destiny, cont'): restProcs -- if get, put it in the front
                          _ -> restProcs ++ [Proc (this,destiny,cont')]) -- otherwise, put it in the back
                       pt 
       -- Assign (New) -> M.insert (counter-1) []
  pt'' = case mExtraProc of
           Nothing -> pt'
           Just extraProc@(Proc (obj,_,_)) -> M.insertWith (flip (++)) obj [extraProc] pt'
  in Just (h', pt'', g')
