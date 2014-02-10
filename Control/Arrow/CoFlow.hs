module CoFlow where

import Control.Comonad
import Control.Monad
import Control.Arrow.Flow (Flow(..), Stalled)


newtype CoFlow w m i o = CoFlow {deCoFlow :: (i -> w (m (Stalled o, (CoFlow w m i o))))}


runCoFlow :: (Comonad w, Monad m) => CoFlow w m i o -> Flow m i o
runCoFlow (CoFlow cflow) = Flow $ \i -> do
                                          (stall, cflow') <- extract (cflow i)
                                          return (stall, runCoFlow cflow') 

                                        
                                        
