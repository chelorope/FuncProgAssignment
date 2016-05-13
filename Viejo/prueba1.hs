import Control.Monad.State -- first, import the state monad

type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  

push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int  
stackManip = do  
    pop