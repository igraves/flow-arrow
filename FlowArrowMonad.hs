module FlowArrowMonad where

import Control.Category
import Control.Arrow
import Prelude hiding (id,(.))

newtype Flow m i o = Flow { deFlow :: (i -> m (Maybe o,Flow m i o)) }

instance Monad m => Category (Flow m) where
  id = Flow (\ i -> return (Just i,id))
  a . b = (flip (<//>)) a b

(<//>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
Flow k1 <//> Flow k2 = Flow (\ i -> do (mn,k1') <- k1 i
                                       case mn of
                                         Just n -> do (o,k2') <- k2 n
                                                      return (o,k1' <//> k2')
                                         Nothing -> return (Nothing,k1' <//> Flow k2))

instance Monad m => Arrow (Flow m) where
  arr f = Flow (\ i -> return (Just (f i),arr f))
  first (Flow k) = Flow (\ (i,x) -> do (mo,k') <- k i
                                       case mo of
                                         Just o  -> return (Just (o,x),first k')
                                         Nothing -> return (Nothing,first k'))
