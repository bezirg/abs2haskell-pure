module PP where

import Base

instance Show Heap where
    show (Heap os fs c) = "Heap: {\nObjects: " ++ show os ++ "\n" 
                        ++ "Fut: "++ show fs ++ "\n"
                        ++ "Counter: " ++ show c ++ "}"
                        
instance Show Proc where
    show (Proc (this,destiny,_cont)) = show (this,destiny,"<fun>")
