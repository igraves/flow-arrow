module Counter where

import Control.Arrow.Flow
import Control.Monad.State


type Count = StateT Int IO

incr :: Count ()
incr = get >>= \x -> put (x+1)


--This is a counting flow node that keeps a count of things
--that have flowed through it

counter :: Flow Count a (a,Int)
counter = Flow $ \input -> do
                              incr
                              c <- get
                              return (Just (input,c), counter)
				

printer :: Show a => Flow Count a ()
printer = Flow $ \input -> do
                              liftIO $ print input
                              return (Just (), printer)

--This computation takes an input that is a member of Show
--Each input gets counted by counter and then the piped through to printer
--which prints out the input.  The resulting real output is a list of [()]
--
--The function uses a foldFlow which is a left-fold over a member of Foldable 
--that produces a list of results.  This is, in effect, like a giant map.
runcomp = evalStateT (liftM fst $ foldFlow (counter <//> printer) [0..5]) 0 
