{-# LANGUAGE GADTs #-}

module Sand where

import Control.Monad
import Control.Monad.Operational ( Program
                                 , ProgramViewT(Return, (:>>=))
                                 , ProgramView
                                 , view
                                 , viewT
                                 , liftProgram
                                 , singleton)

-- type CustomMonad a = Program (Instruction) a
-- data Instruction a where
--   Value :: a -> Instruction a
--   Add   :: Instruction a -> Instruction a -> Instruction a

-- -- a = Add (Value 1) (Value 3)

-- interpretMath :: CustomMonad Int -> Int
-- interpretMath (Return x) = x
  
data StateOp st retVal where
  Get :: StateOp st st
  Put :: st -> StateOp st ()

type State st retVal = Program (StateOp st) retVal  

interpret :: State st retVal -> st -> (st, retVal)
interpret (Get :>>= bar) st = interpret (bar st) st


{-  
--------------------------------------------------
-- Stack

data StackInstruction a where
       Push :: Int -> StackInstruction ()
       Pop  :: StackInstruction Int

type StackProgram a = Program StackInstruction a
type Stack b        = [b]
  
  
interpret :: StackProgram a -> (Stack Int -> a)
interpret = eval . view
       where
         eval :: ProgramView StackInstruction a -> (Stack Int -> a)
         eval (Push a :>>= is) stack     = interpret (is ()) (a:stack)
         eval (Pop    :>>= is) (a:stack) = interpret (is a ) stack
         eval (Return a)       stack     = a  

pop = singleton Pop
push = singleton . Push

i = (interpret $ do
  a <- pop
  b <- pop
  return $ a + b) [3,5,7,11,13]
-}

main = undefined
