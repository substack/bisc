module VM.Util where

import Control.Monad (liftM)

untilMM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
untilMM cond update value = do
    result <- cond value
    if result
        then return value
        else untilMM cond update =<< update value

untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM cond = untilMM $ return . cond

untilM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
untilM_ cond update value = untilM cond update value >> return ()

whileMM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
whileMM cond = untilMM $ (liftM not) . cond

whileM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
whileM cond = untilM (not . cond)

whileM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
whileM_ cond = untilM_ (not . cond)
