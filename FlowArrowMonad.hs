{-# LANGUAGE RecursiveDo #-}

module FlowArrowMonad where
import Control.Category
import Control.Arrow
import Data.Monoid
import Prelude hiding (id,(.))
import Control.Monad.Fix
import Control.Monad.Identity hiding (when)
import Control.Monad.Trans
import Control.Applicative

{- | The Flow type for sequencing monadic computations.  
 -   Underlying monad m, input type i, and output type o.  
 -   Flow is a function that takes the input i and returns
 -   a pair of Maybe output and a (potentially updated) version
 -   of itself to call against again in the future.  The output is
 -   Maybe o instead of o to reflect that some calls against a Flow
 -   may not always readily return a value.  In situations like this,
 -   one should take the updated flow and call it again to try for a
 -   result in a future call.  This feature enables the modeling of 
 -   things like stalls or a "currently empty buffer" in a stream.  The 
 -   instances of Flow in the various typeclasses below assume this 
 -   behavior and implement it.
 -}
newtype Flow m i o = Flow { deFlow :: (i -> m (Maybe o,Flow m i o)) }

-- Instance of Category for Arrow support
instance Monad m => Category (Flow m) where
  id = Flow (\ i -> return (Just i,id))
  a . b = (flip (<//>)) a b

{- | Flow sequence operator.  Takes two flows and chains them left to right.
 -   If the left hand side yields a nothing output, the sequence operator is
 -   executed against the updated version of the left hand argument to try
 -   again for a non-nothing output.
 -}
(<//>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
f@(Flow k1) <//> g@(Flow k2) = Flow (\ i -> do (mn,f') <- k1 i
                                               case mn of
                                                 Just n -> do (o,g') <- k2 n
                                                              return (o,f' <//> g')
                                                 Nothing -> return (Nothing,f' <//> g))

-- (Flow m) is an instance of Arrow.  (Flow m) i o is analogous to 
-- the Arrow a => a b c
instance Monad m => Arrow (Flow m) where
  arr f = Flow (\ i -> return (Just (f i),arr f))
  first (Flow k) = Flow (\ (i,x) -> do (mo,f') <- k i
                                       case mo of
                                         Just o  -> return (Just (o,x),first f')
                                         Nothing -> return (Nothing,first f'))

-- Instance of ArrowChoice which gives choice operators for Arrows.
instance Monad m => ArrowChoice (Flow m) where
  left f@(Flow k) = Flow (\ ei -> case ei of
                                    Left i  -> do (mn,f') <- k i
                                                  case mn of
                                                    Just n  -> return (Just (Left n),left f')
                                                    Nothing -> return (Nothing,left f')
                                    Right i -> return (Just (Right i),left f))

-- ArrowLoop is for computations that that feed back results into themselves
-- This was defined by Adam's genius for Flows over MonadFix.
instance MonadFix m => ArrowLoop (Flow m) where
  loop (Flow k) = Flow (\ b -> mdo (mr@(~(Just (c,d))),f') <- k (b,d)
                                   case mr of
                                     Just _  -> return (Just c,loop f')
                                     Nothing -> return (Nothing,loop f'))


--If the underlying monad is MonadPlus, this feature rises to the Arrow level.
instance MonadPlus m => ArrowZero (Flow m) where
  zeroArrow = Flow (\ _ -> mzero)

instance MonadPlus m => ArrowPlus (Flow m) where
  (Flow k1) <+> (Flow k2) = Flow $ \ i -> do (k1 i `mplus` k2 i)


--The "chaining" model is also a Functor.  fmap is equivalent to lifting a pure
--function into Flow and "appending it to the end of a computation" using the
--arr function to lift pure functions into an Arrow.
instance Monad m => Functor (Flow m i) where
  fmap f k1 = k1 <//> arr f


{-Is an applicative functor as well.  "Pure" is taking a value and forming a Flow
 - that returns a pair with the value as the output and a resuming computation that 
 - recursively does the same thing.  
 -
 - <*> :: f (a -> b) -> f a -> f b in Flow forms a Flow that runs the left hand side 
 - until it yields a function (a -> b) and then tries to run the right hand side until 
 - it gets an (a) and then applies the function for the result.
 -}
instance Monad m => Applicative (Flow m i) where
  pure a  = Flow (\ _ -> return (Just a, pure a)) 
  (Flow f) <*> (Flow g) = Flow (\ i -> do (mf, f') <- f i
                                          case mf of
                                               Nothing -> return (Nothing,f' <*> (Flow g))
                                               Just fo -> do 
                                                                  (mg,g') <- g i
                                                                  case mg of
                                                                       Nothing -> return (Nothing,(Flow f) <*> g')
                                                                       Just go -> return (Just (fo go), f' <*> g'))
                                                                   
                                           
                                                 
--You can treat Flow as a Monad.  Binding is a little strange, though.  The input
--given to run the right is also fed as the input to run the left after the left is 
--computed from the output given by running the right.  Not sure if this follows the 
--Monad laws.
instance Monad m => Monad (Flow m i) where                                                
  return o = Flow (\ _ -> return (Just o, return o))
  Flow k1 >>= g = Flow (\ i -> do (mn,f') <- k1 i
                                  case mn of
                                    Nothing -> return (Nothing,f' >>= g)
                                    Just n  -> deFlow (g n) i)

--Flow is also a monad transformer.  
liftFlow :: Monad m => m o -> Flow m i o
liftFlow m = Flow (\ _ -> do o <- m
                             return (Just o,liftFlow m))

-- The underneath is a redefining of Flow so it can be fit into the MonadTrans type
-- as it's laid out in the MTL.
newtype FlowT i m o = FlowT { deFlowT :: Flow m i o }

instance MonadTrans (FlowT i) where
  lift = FlowT . liftFlow

instance Monad m => Monad (FlowT i m) where
  return  = FlowT . return
  m >>= f = FlowT (deFlowT m >>= deFlowT . f)


--Predicate Flow filters
{-| Constructs a Flow by a given predicate. Stalls whenever input fails predicate test. -}
when :: Monad m => (i -> Bool) -> Flow m i i
when pred = Flow (\ input -> do
                              case pred input of
                                   True -> return (Just input,when pred)
                                   _    -> return (Nothing,when pred))

{-| Filters the output of a Flow by a predicate -}
whenever :: Monad m => Flow m i o -> (o -> Bool) -> Flow m i o
whenever f p = f <//> (when p)

{-| Two-Flow Round-Robin merge.  Runs the first flow until it produces an output, then
 -  switches to the second Flow and does the same before returning to the first. -}
rr2 :: Monad m => Flow m i o -> Flow m i o -> Flow m i o
rr2 f g = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                              case mo of
                                   Nothing -> return (mo, f')
                                   Just o  -> return (Just o, rr2 g f'))
                            
{-| Like rr2, but with three Flows-}
rr3 :: Monad m => Flow m i o -> Flow m i o -> Flow m i o -> Flow m i o
rr3 f g h = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                                case mo of
                                     Nothing -> return (mo, f')
                                     Just o  -> return (Just o, rr3 g h f'))

{-| Like rr2, but with four Flows-}
rr4 :: Monad m => Flow m i o -> Flow m i o -> 
                  Flow m i o -> Flow m i o -> Flow m i o
rr4 f g h i = Flow (\ input -> do (mo, f') <- (deFlow f) input 
                                  case mo of
                                       Nothing -> return (mo, f')
                                       Just o  -> return (Just o, rr4 g h i f'))
