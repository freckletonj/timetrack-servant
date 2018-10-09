{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module Sand where

import Control.Monad
-- import Control.Monad.Operational ( Program
--                                  , ProgramViewT(Return, (:>>=))
--                                  , ProgramView
--                                  , view
--                                  , viewT
--                                  , liftProgram
--                                  , singleton)
--import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Comonad.Cofree
import Control.Monad.Operational


{-
# Contents

* A Simple AST
* Free Monad
* Cofree interpreter
  * https://abailly.github.io/posts/free.html
  * http://dlaing.org/cofun/posts/free_and_cofree.html
* Church Encoded Free
* CoDensity
* Operational
* Freer
  * apparently uses a lot of kleisli arrows
* Free Applicative
  * https://arxiv.org/abs/1403.0749
  * https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html

* Misc
  * Good slides - https://www.andres-loeh.de/Free.pdf
-- -}

{---------------------------------------------------
# AdHoc DSL - https://www.andres-loeh.de/Free.pdf
-}
{-

data Expr = Lit Int | Add Expr Expr

prog = Add (Lit 1) (Add (Lit 2) (Lit 3))

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

mockText :: Expr -> String
mockText (Lit n) = show n
mockText (Add e1 e2) = "(" ++ mockText e1 ++ " + " ++ mockText e2 ++ ")"

-- -}

{---------------------------------------------------
# Free Monad, MultiParamTypeclass DSL -}

{-

-- DSL Operations
data InteractionOp :: * -> * where
      Say :: String -> (() -> r) -> InteractionOp r
      Ask :: (String -> r)       -> InteractionOp r

-- we should be able to derive the Functor instance
instance Functor InteractionOp where
  fmap f (Say msg k) = Say msg (f . k)
  fmap f (Ask k)     = Ask (f . k)
      
type Interaction = Free InteractionOp

-- helper fns
say :: String -> Interaction ()
say msg = Free (Say msg Pure)

ask :: Interaction String
ask = Free (Ask Pure)

-- IO Interpreter
runIO :: Interaction a -> IO a
runIO (Pure x)           = return x
runIO (Free (Say msg k)) = putStrLn msg >>= runIO . k
runIO (Free (Ask k))     = getLine      >>= runIO . k

-- Mock Interpreter
runMock :: (Show a) => Interaction a -> [String] -> [String]
runMock (Pure x)           is = []
runMock (Free (Say msg k)) is = msg : runMock (k ()) is
runMock (Free (Ask k)) (i:is) = runMock (k i) is

-- example
prog = do
  a <- ask
  say $ "hi " ++ a

-- example without do notation
prog' = ask
    >>= say . (++ "hi ")

-- RUN THESE
exi = runIO prog
exm = runMock prog ["joe"]
-- -}


{---------------------------------------------------
# Free + CoFree Interpreter -}

{-
data AskerF k =
  Ask k
  | Say String k
  deriving Functor

type AskerT m a = FreeT AskerF m a

ask :: Monad m => AskerT m ()
ask = liftF $ Ask ()

say :: Monad m => String -> AskerT m ()
say x = liftF $ Say x ()

data CoAskerF k = CoAskerF {
  askH :: k
  , sayH :: String -> k
  } deriving Functor

type CoAsker a = Cofree CoAskerF a

mkCoAsker :: [String] -> CoAsker [String]
mkCoAsker inputs = coiter next start -- coiter :: Functor f => (a -> f a) -> a -> Cofree f a
  where
    next w = CoAskerF (coAsk w) (coSay w)
    start  = [""]

    coAsk :: [String] -> [String]
    coAsk r = r

    coSay :: [String] -> String -> [String]
    coSay r x = x : r
-- -}


{---------------------------------------------------
# Operational DSL
-}
-- {-

-- The DSL
data DSLInstructions a where
  Ask :: DSLInstructions String
  Say :: String -> (DSLInstructions ())

type DSL a = Program DSLInstructions a

-- The Interpreter
evalDSL :: DSL a -> IO a
evalDSL = eval . view
  where
    eval :: ProgramView DSLInstructions a -> IO a
    eval (Ask :>>= is) = getLine >>= (\x -> evalDSL (is x))
    eval (Say x :>>= is) = putStrLn x >> evalDSL (is ())
    eval (Return x) = return x
                                         
mockDSL :: [String] -> DSL a -> [String]
mockDSL inputs = eval . view
  where
    eval :: ProgramView DSLInstructions a -> [String]
    eval (Ask :>>= instructions) = mockDSL
                                   (tail inputs) -- next inputs
                                   (instructions (head inputs)) -- next instructions
    eval (Say x :>>= instructions) = x : mockDSL inputs (instructions ())
    eval (Return _) = []

-- Helper Fns
say :: String -> DSL ()
say msg = singleton (Say msg)

ask :: DSL String
ask = singleton Ask

-- A Program written in the monadic DSL
prog = do
  x <- ask
  say $ "hi " ++ x
  y <- ask
  say $ "hola " ++ y

-- Running it
runEval = evalDSL prog
runMock = mockDSL ["joe", "jose"] prog

    

-- -}
