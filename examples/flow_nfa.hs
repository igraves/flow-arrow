module NFA where
import Control.Arrow
import Control.Arrow.Flow
import Control.Monad.Reader
import Data.Foldable
import Prelude hiding (foldl, foldr)


type RMachine a = Flow (Reader a) Bool Bool

match :: Eq a => a -> RMachine a
match a = Flow $ \input ->
                           --Stall so we can initialize our flip flop
                           return (stalled, match_ input a)
  where
      match_ :: Eq a => Bool -> a -> RMachine a
      match_ ff a = Flow $ \presult -> do
                                          input <- ask --Read the character from the "character bank"
                                          let output = ff && (a == input)
                                          return (finished output, match_ presult a) 

--An alternative to stalling is setting the intial values
--of flip flops.  These values vary based on the position 
--of the matcher relative to the front.  Stalling is a more
--general solution, in my opinion.
--match0 :: Eq a => a -> RMachine a
--match0 = match_ False 
--match1 :: Eq a => a -> RMachine a
--match1 = match_ True 

(<|>) :: Eq a => RMachine a -> RMachine a -> RMachine a
r1 <|> r2 = Flow $ \presult -> do 
                                  (res, cont) <- deFlow (r1 *** r2) (presult,presult)
                                  case deStall res of
                                       Nothing -> return (stalled, step cont)
                                       Just (a,b)  -> return (finished (a || b), step cont)
    where
      step flow = Flow $ \presult -> do
                                        (res, cont) <- deFlow flow (presult, presult)
                                        case deStall res of 
                                             Nothing -> return (stalled, step cont)
                                             Just (a,b)  -> return (finished (a || b), step cont) 

--r1r2 when r1 and r2 are Flows, is just (>>>) 
--Placeholder operation for appending two patterns
-- <++> :: Eq a => RMachine a -> RMachine a -> RMachine a
-- r1 <+> r2 = r1 >>> r2 


star :: Eq a => RMachine a -> RMachine a
star r = star' False r 
    where
      star' outstate r1  = Flow $ \input -> do
                                              let input' = outstate || input
                                              (res, cont) <- deFlow r1 input'
                                              case deStall res of
                                                    Nothing -> return (stalled, star' outstate cont) --recheck
                                                    Just r  -> return (finished (r || input), star' r cont)


--Paper Example from Figure 7 is ((a|b)*)(cd)
example :: RMachine Char 
example = (star ((match 'a') <|> (match 'b'))) >>> ((match 'c') >>> (match 'd'))

--Run a Machine over some Foldable Input from end to end for a match.
runRMachine :: (Eq a, Foldable t) => RMachine a -> t a -> (Bool, RMachine a)
runRMachine rm stream = foldl (\(_,m) c -> stepRMachine m c) (True,rm) stream

--Step an RMachine through one complete cycle until some output is yielded
--Skipping Stalls until that output is encountered
stepRMachine :: (Eq a) => RMachine a -> a -> (Bool, RMachine a)
stepRMachine (Flow f) c = let (res, cont) = runReader (f True) c
                               in case deStall res of 
                                          Nothing -> stepRMachine cont c -- Not necessary
                                          Just a  -> (a,cont) 

                                      
