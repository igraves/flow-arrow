{-# LANGUAGE RecursiveDo #-}

module FlowArrowMonad where
import Control.Category
import Control.Arrow
import Data.Monoid
import Prelude hiding (id,(.))
import Control.Monad.Fix
import Control.Monad.Identity
import Debug.Trace

newtype Flow m i o = Flow { deFlow :: (i -> m (Maybe o,Flow m i o)) }

instance Monad m => Category (Flow m) where
  id = Flow (\ i -> return (Just i,id))
  a . b = (flip (<//>)) a b

(<//>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
f@(Flow k1) <//> g@(Flow k2) = Flow (\ i -> do (mn,f') <- k1 i
                                               case mn of
                                                 Just n -> do (o,g') <- k2 n
                                                              return (o,f' <//> g')
                                                 Nothing -> return (Nothing,f' <//> g))

instance Monad m => Arrow (Flow m) where
  arr f = Flow (\ i -> return (Just (f i),arr f))
  first (Flow k) = Flow (\ (i,x) -> do (mo,f') <- k i
                                       case mo of
                                         Just o  -> return (Just (o,x),first f')
                                         Nothing -> return (Nothing,first f'))


instance Monad m => ArrowChoice (Flow m) where
  left f@(Flow k) = Flow (\ ei -> case ei of
                                    Left i  -> do (mn,f') <- k i
                                                  case mn of
                                                    Just n  -> return (Just (Left n),left f')
                                                    Nothing -> return (Nothing,left f')
                                    Right i -> return (Just (Right i),left f))

instance MonadFix m => ArrowLoop (Flow m) where
  loop (Flow k) = Flow (\ b -> mdo (mr@(~(Just (c,d))),f') <- k (b,d)
                                   case mr of
                                     Just _  -> return (Just c,loop f')
                                     Nothing -> return (Nothing,loop f'))


instance MonadPlus m => ArrowZero (Flow m) where
  zeroArrow = Flow (\ _ -> mzero)


instance MonadPlus m => ArrowPlus (Flow m) where
  (Flow k1) <+> (Flow k2) = Flow $ \ i -> do (k1 i `mplus` k2 i)

instance Monad m => Functor (Flow m i) where
  fmap f k1 = k1 <//> arr f
                                                 
instance Monad m => Monad (Flow m i) where                                                
  return o = Flow (\ _ -> return (Just o, return o))
  (Flow k1) >>= g = Flow (\ i ->do (mn,f') <- k1 i
                                   case mn of
                                        Nothing -> return (Nothing, f' >>= g)
                                        Just n -> return (undefined, g n))
                                                     
                      
