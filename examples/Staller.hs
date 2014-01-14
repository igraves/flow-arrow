module Staller where

import Control.Arrow.Flow
import Control.Monad


produce1 :: Monad m => Flow m a Int
produce1 = stall $ Flow $ \_ -> return (finished 1, produce1)


incr :: Monad m => Flow m Int Int
incr = stall $ Flow $ \i -> return (finished (i + 1),incr)




simulate :: (Show s)  => Flow IO () s -> IO ()
simulate flow = do
                  (res, new) <- deFlow flow ()
                  putStrLn $ "Result: " ++ (show res)
                  putStrLn "<Enter> to continue"
                  getLine
                  simulate new
                  
main = simulate (produce1 </> incr)
