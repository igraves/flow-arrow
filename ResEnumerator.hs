{-#LANGUAGE RankNTypes #-}

module REnumerator where

import Control.Monad.Resumption.Reactive
import Control.Category
import Control.Arrow
import Prelude hiding ((.),id)


newtype Stream m a b = Stream { deStream :: forall c. a -> ReacT a b m c}

instance Monad m => Category (Stream m) where
  id = Stream $ preact id 
  a . b = (flip (<//>)) a b 

instance Monad m => Arrow (Stream m) where
  arr f = Stream $ preact f 
  first stream = Stream $ \(input,copy) -> ReacT $ do
                                                    cmp <- deReacT $ (deStream stream) input
                                                    case cmp of
                                                          Left v -> return $ Left v 
                                                          Right (output,f) -> return $ Right ((output,copy),endrun f)
        where
          --endrun :: Monad m => (a -> ReacT a b m c) -> ((a,d) -> ReacT (a,d) (b,d) m c)
          endrun res = \(input,copy) -> ReacT $ do
                                                  cmp <- deReacT $ res input
                                                  case cmp of
                                                       Left v -> return $ Left v 
                                                       Right (output,res') -> return $ Right ((output,copy), endrun res')


(/>) :: (Monad m) => ReacT a b m c -> ReacT b d m c -> (a -> ReacT a d m c)
a /> b = \input ->   ReacT $ do
                           l <- deReacT a
                           case l of 
                                Left  term -> return $ Left term 
                                Right cl   -> do
                                                r <- deReacT b
                                                case r of
                                                     Left  term' -> return $ Left term'
                                                     Right cr    -> do
                                                                      (lresult,lcmp) <- runTick (cl) input --These aren't correct, knot is correct, however.
                                                                      (rresult,rcmp) <- runTick (cr) lresult
                                                                      return $ Right (rresult,lcmp `knot` rcmp)
      where
          runTick (_, f) inp = do 
                                 r <- deReacT $ f inp
                                 return $ fromRight r
          fromRight (Right r) = r

(<//>) :: Monad m => Stream m a b -> Stream m b c -> Stream m a c
a <//> b = Stream $ (deStream a) `knot` (deStream b) 

--Knot takes two functions producing ReacT of chainable signals
--and merges them into one reactive resumption
knot :: Monad m => (input -> ReacT input output1 m a) -> 
                   (output1 -> ReacT output1 output m a) -> 
                   (input -> ReacT input output m a)

knot lre rre = \input -> ReacT $ do
                                   lre' <- deReacT $ lre input
                                   case lre' of
                                          Left (res) -> return $ Left res 
                                          Right (loutput,lres) -> do
                                                                    rre' <- deReacT $ rre loutput
                                                                    case rre' of
                                                                           Left (res) -> return $  Left res
                                                                           Right (routput,rres) -> return $ Right (routput, lres `knot` rres) 
                            

preact :: Monad m => (input -> output) -> (input -> ReacT input output m term)
preact f = \input -> ReacT $ return $ Right (f input, preact f)
