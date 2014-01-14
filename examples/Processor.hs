{-# LANGUAGE TemplateHaskell #-}
module Processor where
import Prelude hiding (and, read, set)
import Control.Arrow.Flow
import Control.Monad.State
import Control.Lens hiding (set)
import Data.Word
import Data.Bits
import Control.Applicative

{-
 -
 - Add  --> Add A to B, Store in A
 - And  --> And A and B, Store in A
 - ZeroA --> Zeroes A
 - IncrA --> Increments A
 - Sto  --> Store A to some address specified by index
 - Read --> Read from some address in index from memory to B
 - Jump --> Jump to the address in index 
 - Set  --> Set Index register to value stored in A
 -
 - Memory reads take 2 cycles.  Writes take 2 cycles.  Ariths take 1 cycle.
 - Jumps take 1 cycle. Fetches and decodes are free.
 -}
data Ops = Add | Sto | Read | Jump | Set | And | ZeroA | IncrA | Err deriving (Show, Eq)
type Register = Word16

type MState = StateT Machine IO

data Machine = Machine {
                  _a :: Register,
                  _b :: Register,
                  _i :: Register,
                  _pc :: Register,
                  _mem :: [(Word16,Word16)],
                  _code :: [(Word16,Ops)]
               } 


makeLenses ''Machine

instance Show Machine where
  show m = "{"
            ++ "\n     A:  " ++ show ((^.a) m) 
            ++ "\n     B:  " ++ show ((^.b) m) 
            ++ "\n     I:  " ++ show ((^.i) m) 
            ++ "\n     PC: " ++ show ((^.pc) m) 
            ++ "\n}"

updateMem c [] = [c]
updateMem c@(a,b) (z@(x,_):xs) = if a == x then (a,b):xs else z : updateMem c xs

lookupMem w ((w',m):xs) = if w == w' then m else lookupMem w xs
lookupMem x [] = error $ show x 

--Computations in State without control flow

add_ :: MState ()
add_ = do
         ra <- use a
         rb <- use b
         a .= ra + rb

store_ :: MState ()
store_ = do
          ri <- use i
          ra <- use a
          m  <- use mem
          mem .= updateMem (ri,ra) m

read_ :: MState ()
read_ = do
          ri <- use i
          m  <- use mem
          b .= lookupMem ri m

jump_ :: MState ()
jump_ = do
          ra <- use a
          pc .= ra
          rpc <- use pc
          liftIO $ print $ "PC PC PC: " ++ (show rpc)

set_ :: MState ()
set_ = do
         ra <- use a
         i .= ra

and_ :: MState ()
and_ = do
         ra <- use a
         rb <- use b
         a .= ra .&. rb

zeroA_ :: MState ()
zeroA_ = do
          a .= 0x0

incrA_ :: MState ()
incrA_ = do
          ra <- use a
          a .= ra + 0x1
err_ :: MState ()
err_ = return ()

--Flows for each instruction bundle their stateful-layer computations in each of these
--execution flows

add :: Flow MState i ()
add = stall $ liftFlow add_

store :: Flow MState i ()
store = stall2 $ liftFlow store_

read :: Flow MState i ()
read = stall2 $ liftFlow read_ 

jump :: Flow MState i ()
jump =  stall $ liftFlow jump_

set :: Flow MState i ()
set = stall $ liftFlow set_

and :: Flow MState i ()
and = stall $ liftFlow and_

zeroA :: Flow MState i ()
zeroA = stall $ liftFlow zeroA_

incrA :: Flow MState i ()
incrA = stall $ liftFlow incrA_

err :: Flow MState i ()
err = stall $ liftFlow err_

--A decoder will "decode" (aka dispatch) the right execution path
--data Ops = Add | Sto | Read | Jump | Set | And | ZeroA | IncrA 

decode_execute = parchoice [
            ((==And), and),
            ((==Sto), store),
            ((==Read), read),
            ((==Jump), jump),
            ((==Set), set),
            ((==ZeroA), zeroA),
            ((==IncrA), incrA),
            (\_ -> True, err)
          ]
{-
decode_execute :: Flow MState Ops ()
decode_execute = Flow $ \opcode -> do 
                                      liftIO $ print $ "--INSIDE DECODE-EXEC"
                                      liftIO $ print opcode
                                      liftIO $ print $ "INSIDE DECODE-EXEC--"
                                      let next = case opcode of
                                                        And   -> and
                                                        Sto   -> store
                                                        Read  -> read
                                                        Jump  -> jump
                                                        Set   -> set
                                                        ZeroA -> zeroA
                                                        IncrA -> incrA
                                      return (Nothing, (pure id `viewer` next)) 
-}

fetch :: Flow MState () Ops
fetch = Flow $ \() -> do
                       c <- use code 
                       rpc <- use pc
                       let op = lookupMem rpc c
                       pc .= rpc + 1
                       return (finished op, fetch)

processor = fetch `viewer` decode_execute 

--Testing routines
test_machine = Machine {
                            _a = 0,
                            _b = 0,
                            _i = 0,
                            _pc = 0,
                            _mem = zip [0x0000..0xFFFF] (repeat 0),
                            _code = zip [0..] $ [
                                                  IncrA,
                                                  ZeroA,
                                                  Jump
                                                ]
                             
                       }
viewer :: (MonadIO m, Show s) => Flow (StateT s m) a b -> Flow (StateT s m) b c -> Flow (StateT s m) a c 
first `viewer` next = let vflow = Flow $ \i -> do 
                                              m <- get 
                                              liftIO $ print m
                                              liftIO $ putStrLn "<Enter to continue>"
                                              liftIO getLine 
                                              return (finished i, vflow)
                       in first </> vflow </> next

--simulate :: Monad m => s -> Flow (StateT s m) () o -> m b
simulate :: Machine -> Flow MState () () -> IO b
simulate s flow = do
                          ((result, cont),s') <- step s flow
                          print s'
                          simulate s' cont 

step :: Machine -> Flow MState () () -> IO ((Stalled (),Flow MState () ()), Machine)
step s flow = do
                r@((result, cont),s') <- runStateT (deFlow flow ()) s 
                case deStall result of
                     Nothing -> do
                                  print s' 
                                  step s' cont
                     Just ()  -> do
                                   return r
                                  
                          

main :: IO ()
main = do
          putStrLn "Starting simulation..."
          simulate test_machine (processor) -- </> (flow_viewer processor)) 
