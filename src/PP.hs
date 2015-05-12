-- | for debugging: Show-instances of heaps and processes 
module PP where

import Base
import Data.Sequence (Seq)
import qualified Data.Vector.Mutable as M
import Control.Monad (foldM)
import qualified Data.Vector as V (freeze, filter, indexed)

printHeap :: Heap -> IO ()
printHeap (Heap os fs c) = do
  putStrLn "Heap: {"
  putStr "    Objects:"
  oList <- foldM (\ acc i -> (do
                             res <- os `M.read` i
                             case res of
                               Nothing -> return acc
                               Just (attrs, pqueue) -> do
                                        fattrs <- V.freeze attrs
                                        return $ show (i, (V.filter (\ (_, v) -> v /= (-1)) $ V.indexed fattrs, pqueue)) : acc
                                )) [] [0..c-1]
  fList <- foldM (\ acc i -> do
                   res <- fs `M.read` i
                   return $ maybe acc (\ x-> show (i,x) : acc) res) [] [0..c-1]

  putStrLn (concat oList)
  putStr "    Futures:"
  putStrLn (concat fList)
  putStrLn ("    Counter: " ++ show c)
  putStrLn "}"

instance Show Proc where
    show (Proc (destiny,_cont)) = show (destiny,"<fun>")
