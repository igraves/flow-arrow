{-# LANGUAGE RecursiveDo, ScopedTypeVariables #-}

module Control.Arrow.Flow where
import Control.Category
import Control.Arrow
import Data.Monoid
import Data.Foldable hiding (any)
import Prelude hiding (id,(.),foldr,foldl)
import Control.Monad.Fix
import Control.Monad.Identity hiding (when)
import Control.Monad.Trans
import Control.Applicative
import Data.Maybe (isJust, catMaybes, fromJust)

{- | The stalled type is isomorphic to Maybe.  We keep the two 
 -   separated here for bookkeeping purposes.  Some computations
 -   can return a value wrapped in a Maybe and this keeps confusing
 -   Maybe (Maybe a)'s from happening.
 -}
newtype Stalled a = Stalled {deStall :: (Maybe a)} 


instance Show s => Show (Stalled s) where
  show (Stalled Nothing) = "Stalled"
  show (Stalled a) = show a

{-| Convenience function for stalling. -}

stalled :: Stalled a
stalled = Stalled $ Nothing

{-| Convenience function for returning a value -}

finished :: a -> Stalled a
finished a = Stalled $ Just a

instance Functor Stalled where
  fmap f stall = case deStall stall of 
                      Nothing  -> Stalled Nothing
                      (Just a) -> Stalled $ Just $ f a

{- | The Flow type for sequencing monadic computations.  
    Underlying monad m, input type i, and output type o.  
    Flow is a function that takes the input i and returns
    a pair of Maybe output and a (potentially updated) version
    of itself to call against again in the future.  The output is
    Maybe o instead of o to reflect that some calls against a Flow
    may not always readily return a value.  In situations like this,
    one should take the updated flow and call it again to try for a
    result in a future call.  This feature enables the modeling of 
    things like stalls or a "currently empty buffer" in a stream.  The 
    instances of Flow in the various typeclasses below assume this 
    behavior and implement it.
 -}
newtype Flow m i o = Flow { deFlow :: (i -> m (Stalled o,Flow m i o)) }

-- Instance of Category for Arrow support
instance Monad m => Category (Flow m) where
  id = Flow (\ i -> return (finished i,id))
  a . b = (flip (</>)) a b

{- | Flow sequence operator.  Takes two flows and chains them left to right.
    If the left hand side yields a nothing output, the sequence operator is
    executed against the updated version of the left hand argument to try
    again for a non-nothing output.
 -}
(</>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
f@(Flow k1) </> g@(Flow k2) = Flow (\ i -> do (mn,f') <- k1 i
                                              case deStall mn of
                                                Just n  -> do
                                                              --Try to run g once, if not flip to app_flow
                                                              (rr, g') <- k2 n
                                                              case deStall rr of
                                                                Just n  -> return (rr, f' </> g')
                                                                Nothing -> return (stalled, app_flow n f' g')
                                                Nothing -> return (stalled,f' </> g))
      where
        app_flow :: Monad m => b -> Flow m a b -> Flow m b c -> Flow m a c
        app_flow input left right = Flow (\_ -> do  (res, right') <- deFlow right input
                                                    case deStall res of
                                                         Just _  -> return (res, left </> right')
                                                         Nothing -> return (stalled, app_flow input left right'))

{- | Flow sequence operator.  Takes two flows and chains them left to right.
    If the left hand side yields a nothing output, the sequence operator is
    executed against the updated version of the left hand argument to try
    again for a non-nothing output.  This operator introduces a stall
    between connected flows.
 -}
(<//>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
f@(Flow k1) <//> g@(Flow k2) = Flow (\ i -> do (mn,f') <- k1 i
                                               case deStall mn of
                                                 Just n  -> return (stalled, app_flow n f' g)
                                                 --Just n -> do (o,g') <- k2 n
                                                 --             return (o,f' <//> g') --original 
                                                 Nothing -> return (stalled,f' <//> g))
      where
        app_flow :: Monad m => b -> Flow m a b -> Flow m b c -> Flow m a c
        app_flow input left right = Flow (\_ -> do  (res, right') <- deFlow right input
                                                    case deStall res of
                                                         Just _  -> return (res, left <//> right')
                                                         Nothing -> return (stalled, app_flow input left right'))
                            
-- (Flow m) is an instance of Arrow.  (Flow m) i o is analogous to 
-- the Arrow a => a b c
instance Monad m => Arrow (Flow m) where
  arr f = Flow (\ i -> return (finished (f i),arr f))
  first (Flow k) = Flow (\ (i,x) -> do (mo,f') <- k i
                                       case deStall mo of
                                         Just o  -> return (finished (o,x),first f')
                                         Nothing -> return (stalled,first f'))

-- Instance of ArrowChoice which gives choice operators for Arrows.
instance Monad m => ArrowChoice (Flow m) where
  left f@(Flow k) = Flow (\ ei -> case ei of
                                    Left i  -> do (mn,f') <- k i
                                                  case deStall mn of
                                                    Just n  -> return (finished (Left n),left f')
                                                    Nothing -> return (stalled,left f')
                                    Right i -> return (finished (Right i),left f))

-- ArrowLoop is for computations that that feed back results into themselves
-- This was defined by Adam's genius for Flows over MonadFix.
instance MonadFix m => ArrowLoop (Flow m) where
  loop (Flow k) = Flow (\ b -> mdo (mr@(~(Stalled(Just (c,d)))),f') <- k (b,d)
                                   case deStall mr of
                                     Just _  -> return (finished c,loop f')
                                     Nothing -> return (stalled,loop f'))


--If the underlying monad is MonadPlus, this feature rises to the Arrow level.
instance MonadPlus m => ArrowZero (Flow m) where
  zeroArrow = Flow (\ _ -> mzero)

instance MonadPlus m => ArrowPlus (Flow m) where
  (Flow k1) <+> (Flow k2) = Flow $ \ i -> do (k1 i `mplus` k2 i)


--The "chaining" model is also a Functor.  fmap is equivalent to lifting a pure
--function into Flow and "appending it to the end of a computation" using the
--arr function to lift pure functions into an Arrow.
instance Monad m => Functor (Flow m i) where
  fmap f k1 = k1 </> arr f


{-Is an applicative functor as well.  "Pure" is taking a value and forming a Flow
  that returns a pair with the value as the output and a resuming computation that 
  recursively does the same thing.  
 
  <*> :: f (a -> b) -> f a -> f b in Flow forms a Flow that runs the left hand side 
  until it yields a function (a -> b) and then tries to run the right hand side until 
  it gets an (a) and then applies the function for the result.
 -}
instance Monad m => Applicative (Flow m i) where
  pure a  = Flow (\ _ -> return (finished a, pure a)) 
  (Flow f) <*> (Flow g) = Flow (\ i -> do (mf, f') <- f i
                                          case deStall mf of
                                               Nothing -> return (stalled,f' <*> (Flow g))
                                               Just fo -> do 
                                                                  (mg,g') <- g i
                                                                  case deStall mg of
                                                                       Nothing -> return (stalled,(Flow f) <*> g')
                                                                       Just go -> return (finished (fo go), f' <*> g'))
                                                                   
                                           
                                                 
--You can treat Flow as a Monad.  Binding is a little strange, though.  The input
--given to run the right is also fed as the input to run the left after the left is 
--computed from the output given by running the right.  Not sure if this follows the 
--Monad laws.
instance Monad m => Monad (Flow m i) where                                                
  return o = Flow (\ _ -> return (finished o, return o))
  Flow k1 >>= g = Flow (\ i -> do (mn,f') <- k1 i
                                  case deStall mn of
                                    Nothing -> return (stalled,f' >>= g)
                                    Just n  -> deFlow (g n) i)

{-| Flow is also a monad transformer.  This function performs a lift. -}
liftFlow :: Monad m => m o -> Flow m i o
liftFlow m = Flow (\ _ -> do o <- m
                             return (finished o,liftFlow m))

{-| This is a redefining of Flow so it can be fit into the MonadTrans type  as it's laid out in the MTL.-}
newtype FlowT i m o = FlowT { deFlowT :: Flow m i o }

instance MonadTrans (FlowT i) where
  lift = FlowT . liftFlow

instance Monad m => Monad (FlowT i m) where
  return  = FlowT . return
  m >>= f = FlowT (deFlowT m >>= deFlowT . f)

{-| Flows can be run on a foldable data structure to "termination" -}
foldFlow :: (Monad m, Foldable t) => Flow m a b -> t a -> m ([b], Flow m a b)
foldFlow flow x = foldl ff (return ([], flow)) x
  where
    ff :: (Monad m) => m ([b], Flow m a b) -> a -> m ([b], Flow m a b)
    ff fl val = do
                  (vals,(Flow f)) <- fl
                  r <- f val 
                  case r of
                       (Stalled Nothing,flow') -> ff (return (vals,flow')) val
                       (Stalled (Just x),flow')  -> return (vals ++ [x],flow')

--Routing Flow Combinators
{-| Evaluates two unique flow in parallel and returns their results when they have both completed 
 -  This is a hand-implemented (***) function for Arrows.
 - -}
evalPar :: forall a b c d m. Monad m => Flow m a b -> Flow m c d -> Flow m (a,c) (b,d)
evalPar f1 f2 = evalPar' ((Nothing,f1),(Nothing,f2)) 
  where
    evalPar' :: Monad m => ((Maybe b,Flow m a b),(Maybe d, Flow m c d)) -> Flow m (a,c) (b,d)
    evalPar' wpair@((r1,ff1),(r2,ff2)) = Flow $ \(a,c) -> case (isNothing r1) || (isNothing r2) of
                                                  True -> do 
                                                            wpair' <- step (a,c) wpair
                                                            return (stalled, evalPar' wpair')
                                                  False -> return (finished ((fromJust r1),(fromJust r2)), evalPar ff1 ff2) 
    
    step :: Monad m => (a,c) -> ((Maybe b, Flow m a b),(Maybe d, Flow m c d)) -> m ((Maybe b, Flow m a b),(Maybe d, Flow m c d))
    step _ s@((Just _,_),(Just _,_)) = return s
    step (a,b) (p1,p2) = do 
                           p1' <- runPair a p1
                           p2' <- runPair b p2
                           return (p1',p2') 

    --runPair :: Monad m => a -> (Maybe b, Flow m a b) -> m (Maybe b, Flow m a b)
    --Scoped type variables keeps me from making this explicit :)
    runPair _ s@(Just _,_) = return s
    runPair inp (Nothing, flow) = do
                                 (res, cont) <- deFlow flow inp
                                 case deStall res of
                                      Just n  -> return (Just n, cont)
                                      Nothing -> return (Nothing, cont)
    isNothing Nothing = True
    isNothing _       = False

{-| Evaluates all of the flows in the list in parallel until they've run to completion.  
 -  Blocks until all have completed.
 -}

evalall :: forall a b m. Monad m => [Flow m a b] -> Flow m a [b]
evalall flws = evalall' $ zip (repeat Nothing) flws
  where
    evalall' :: Monad m => [(Maybe b, Flow m a b)] -> Flow m a [b]
    evalall' flows = Flow $ \i -> case any (\(a,_) -> isNothing a) flows of
                                   True -> do 
                                            flows' <- mapM (step i) flows
                                            return (stalled, evalall' flows')
                                   False -> do 
                                            return (finished $ catMaybes $ map fst flows, evalall flws)
    
    step :: Monad m => a -> (Maybe b, Flow m a b) -> m (Maybe b, Flow m a b)
    step _ s@(Just _, _) = return s 
    step inp (Nothing, flow) = do
                                 (res, cont) <- deFlow flow inp
                                 case deStall res of
                                      Just n  -> return (Just n, cont)
                                      Nothing -> return (Nothing, cont)

    isNothing Nothing = True
    isNothing _ = False



{-| Parallel choice combinator takes a list of alternative flows with their enabling
 -  predicate functions.  If more than one function accepts the input, the result is undefined.
 -}
parchoice :: Monad m => [(a -> Bool, Flow m a b)] -> Flow m a b -> Flow m a b
parchoice flows dflt = let totaled = (map (\(a,b) -> gate a b) flows) ++ [(gate (\_ -> True) dflt)]
                        in (evalall totaled) </> match_just
      where
        --This function should be hidden because the totality of the choice is ensured
        --by its wrapper using the "default" function as the second argument
        --This is tacked onto the end of the list and is picked last if all other
        --choices fail
        match_just :: Monad m => Flow m [Maybe a] a
        match_just = Flow $ \i -> return (finished $ fromJust $ fromJust $ find (isJust) i, match_just)

--Predicate Flow filters
{-| Constructs a Flow by a given predicate. Stalls whenever input fails predicate test. -}
when :: Monad m => (i -> Bool) -> Flow m i i
when pred = Flow (\ input -> do
                              case pred input of
                                   True -> return (finished input,when pred)
                                   _    -> return (stalled,when pred))

{-| Gates a flow according to a predicate.  Flow is not run if predicate is false.  
 - Nothing returned instead.
 -}
gate :: Monad m => (i -> Bool) -> Flow m i o -> Flow m i (Maybe o)
gate pred flow = Flow $ \i -> case pred i of
                                   True  -> do 
                                              (res, cont) <- deFlow flow i
                                              return (fmap Just res, gate pred cont)
                                   False -> return (finished Nothing, gate pred flow)

{-| Two-Flow Round-Robin merge.  Runs the first flow until it produces an output, then
 -  switches to the second Flow and does the same before returning to the first. -}
rr2 :: Monad m => Flow m i o -> Flow m i o -> Flow m i o
rr2 f g = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                              case deStall mo of
                                   Nothing -> return (mo, f')
                                   Just o  -> return (finished o, rr2 g f'))
                            
{-| Like rr2, but with three Flows-}
rr3 :: Monad m => Flow m i o -> Flow m i o -> Flow m i o -> Flow m i o
rr3 f g h = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                                case deStall mo of
                                     Nothing -> return (mo, f')
                                     Just o  -> return (finished o, rr3 g h f'))

{-| Like rr2, but with four Flows-}
rr4 :: Monad m => Flow m i o -> Flow m i o -> 
                  Flow m i o -> Flow m i o -> Flow m i o
rr4 f g h i = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                                  case deStall mo of
                                       Nothing -> return (mo, f')
                                       Just o  -> return (finished o, rr4 g h i f'))
{-| Stall underlying Flow one cycle -}
stall :: Monad m => Flow m i o -> Flow m i o 
stall flow = Flow $ \i -> return (stalled,flow)

{-| Stall underlying Flow two cycles  -}
stall2 :: Monad m => Flow m i o -> Flow m i o 
stall2 = stall . stall

stall3 :: Monad m => Flow m i o -> Flow m i o 
stall3 = stall . stall . stall

stall4 :: Monad m => Flow m i o -> Flow m i o 
stall4 = stall . stall . stall . stall

{-| Split flows merging 
 -
 -      __
 - -a->|__|-b->       ____
 -             |--b->|    |
 -                   |    |-e->
 -      __     |--d->|____|
 - -c->|__|-d->
 -
 - -}
merge :: Monad m => Flow m a b -> Flow m c d -> Flow m (b,d) e -> Flow m (a,c) e
merge f1 f2 f3 = (f1 *** f2) </> f3




