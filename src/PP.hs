-- | for debugging: Show-instances of heaps and processes 
module PP where

import Base

instance Show Heap where
    show (Heap os fs c) = "Heap: {\nObjects: " ++ show os ++ "\n" 
                        -- ++ "Fut: "++ show fs ++ "\n"
                        ++ "Counter: " ++ show c ++ "}"
                        
instance Show Proc where
    show (Proc (destiny,_cont)) = show (destiny,"<fun>")
