module REnumerator where

import Control.Monad.Resumption.Reactive

(/>) :: (Monad m) => ReacT a b m c -> ReacT b d m c -> (a -> ReacT a d m c)
a /> b = \input -> ReacT $ do
                           l <- deReacT a
                           case l of 
                                Left  term -> return $ Left term 
                                Right cl   -> do
                                                r <- deReacT b
                                                case r of
                                                     Left  term' -> return $ Left term'
                                                     Right cr    -> do
                                                                      (lresult,lcmp) <- runTick (cl) input
                                                                      (rresult,rcmp) <- runTick (cr) lresult
                                                                      return $ Right (rresult,lcmp `knot` rcmp)
      where
          fromRight (Right r) = r
          runTick (_, f) inp = do 
                                 r <- deReacT $ f inp
                                 return $ fromRight r
          knot lre rre = \input -> ReacT $ do
                                            (loutput,lres) <- (deReacT $ lre input) >>= return . fromRight
                                            (routput,rres) <- (deReacT $ rre loutput) >>= return . fromRight
                                            return $ Right (routput,knot lres rres) 
                            

                           
                
                                
                                                                      

