module Sched where

import Base
import Eval
import PP ()
import System.Random
import qualified Data.Map as M
import Debug.Trace

-- | just initializes a "true" stdgen from the hardware clock
runIO :: Int -> Method -> IO Heap
runIO n mainMethod = do
  g <- getStdGen
  return (run n mainMethod g)

-- | runs with a fixed (default) stdgen
runFixed :: Int -> Method -> Heap
runFixed n mainMethod = run n mainMethod (mkStdGen maxBound)

-- | given a number of iterations and an entrypoint (ABS main-method) , 
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
                      

sched :: Int -> (Heap,ProcTable,StdGen) -> Heap
sched n (h,pt,g) | n < 0 = error "iterations must be positive"
                 | n == 0 = trace ("reached max steps\nLast ProcTable: " ++ show pt) h
                 | otherwise = case step (h,pt,g) of
                                 Nothing -> trace ("finished (empty proctable), " ++ show n ++ "steps left") h
                                 Just conf' -> sched (n-1) conf'

step :: (Heap,ProcTable,StdGen)  -- ^ a kind of configuration
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
