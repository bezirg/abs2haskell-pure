-- | The global scheduler that schedules COGs (single-objects in our case)
-- We simulate non-determinism by pseudo-randomness
module Sched (
              run, sched
             ) where

import Base
import Eval
import PP ()
import Debug.Trace
import qualified Data.Sequence as S
import qualified Data.Vector.Mutable as V

-- | Given a number of iterations and an entrypoint (ABS main-method) , 
-- it executes the program only for that many iterations returning the final heap.
run :: Int -> Method -> IO Heap
run iters mainMethod = do
    let mainObjRef = 0 -- "main" object (0 ref by default)
    let mainFutRef = mainObjRef + 1 -- main-method's destiny (1 ref by default)
    initObjVec <- V.new 10         -- the initial object vector (starting size: 10)
    initAttrVec <- V.replicate 10 (-1)
    (initAttrVec `V.write` 0) (-123) -- the main object only has a default "__main__" attribute to return it in the end
    -- put the main object
    (initObjVec `V.write` mainObjRef) (initAttrVec, S.singleton $ Proc (mainFutRef
                                                                       -- async call to main method
                                                                       ,mainMethod [] mainObjRef Nothing (\ () -> last_main)
                                                                       ))
    initFutVec <- V.new 10    -- the initial future vector (starting size: 10)
    (initFutVec `V.write` mainFutRef) (Left []) -- putting the main unresolved destiny
    let initHeap = Heap { objects = initObjVec
                        , futures = initFutVec
                        , newRef = mainFutRef+1}
    let initSchedQueue = S.singleton mainObjRef
    sched iters (initHeap,initSchedQueue)
    where
      last_main :: Stmt
      last_main = Return 0 Nothing (error "call to main: main is a special block")
                      

-- | The sched is just a looper of the step function
sched :: Int                     -- ^ the max iterations to run
      -> (Heap,SchedQueue) -- ^ an initial program configuration
      -> IO Heap  -- ^ the final heap
sched n (h,pt) | n < 0 = error "iterations must be positive"
               | n == 0 = traceIO ("reached max steps\nLast SchedTable: " ++ show pt) >> return h
               | S.null pt = traceIO ("finished (empty schedtable), " ++ show n ++ "steps left") >> return h
               | otherwise = do
  let (firstObj S.:< restObjs) = S.viewl pt
  (execedStmt, addedSched, h') <- eval firstObj h
  let pt' = foldl (S.|>) restObjs  addedSched
  sched (n-1) (h', pt')
