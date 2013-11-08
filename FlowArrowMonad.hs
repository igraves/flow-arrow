module FlowArrowMonad where

import Control.Category
import Control.Arrow
import Prelude hiding (id,(.))

newtype Flow m i o = Flow { deFlow :: (i -> m (o,Flow m i o)) }

instance Monad m => Category (Flow m) where
  id = Flow (\ i -> return (i,id))
  a . b = (flip (<//>)) a b

(<//>) :: Monad m => Flow m a b -> Flow m b c -> Flow m a c
Flow k1 <//> Flow k2 = Flow (\ i -> do (n,k1') <- k1 i
                                       (o,k2') <- k2 n
                                       return (o,k1' <//> k2'))

instance Monad m => Arrow (Flow m) where
  arr f = Flow (\ i -> return (f i,arr f))
  first (Flow k) = Flow (\ (i,x) -> do (o,k') <- k i
                                       return ((o,x),first k'))
