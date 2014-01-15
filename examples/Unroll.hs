module Unroll where


import Control.Arrow.Flow
import Control.Monad.Resumption.Reactive


unroll :: Monad m => Flow m a b ->  ReacT a (Maybe b) m ()
unroll flow = ReacT $ return $ Right (Nothing,
                                      \input ->ReacT $ do
                                                          (fres,fcont) <- deFlow flow input
                                                          return $ Right (deStall fres, \input -> next input fcont))
      where
        next input flw = ReacT $ do
                                    (fres, fcont) <- deFlow flw input
                                    return $ Right (deStall fres, \input -> next input fcont)
                                            
                        
