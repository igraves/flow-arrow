module HiddenState where

import Control.Arrow.Flow
import Control.Monad.State


newtype SFlow s m a b = SFlow { deSFlow :: (s,Flow (StateT s m) a b) }


bake :: (Monad m) => SFlow s m a b -> Flow m a b
bake (SFlow (s,sflow)) = Flow $ \i -> do
                                          ((out, sflow'),s') <- runStateT (deFlow sflow i) s
                                          case deStall out of
                                                Nothing  -> return (stalled, bake (SFlow (s',sflow')))
                                                Just res -> return (out, bake (SFlow (s',sflow')))
