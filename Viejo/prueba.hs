import Data.Map (Map)
import AwkiSA
import qualified Data.Map as M
import Control.Monad.State

type Stack = [Int]

iniciar :: State Stack ()
iniciar = put []

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

instance Show Int => Show (State Int) where
show (state f) = show (f i)