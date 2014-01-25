module HiddenState where

import Control.Arrow.Flow
import Control.Monad.State


newtype SFlow s m a b = SFlow { deSFlow :: (s,Flow (StateT s m) a b) }


unroll :: (Monad m) => SFlow s m a b -> Flow m a b
unroll (SFlow (s,sflow)) = Flow $ \i -> do
                                          ((out, sflow'),s') <- runStateT (deFlow sflow i) s
                                          case deStall out of
                                                Nothing  -> return (stalled, unroll (SFlow (s',sflow')))
                                                Just res -> return (out, unroll (SFlow (s',sflow')))
