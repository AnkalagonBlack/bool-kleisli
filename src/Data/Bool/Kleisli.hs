module Data.Bool.Kleisli (
allM,
anyM,
orM,
andM,
kleisify
) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe

boolToMaybe :: Monad m => Bool -> m (Maybe ())
boolToMaybe True = return $ Just ()
boolToMaybe False  = return Nothing

helper bs a f = fmap (f . isJust) . runMaybeT . mapM_ (MaybeT . ($ a) . (<=<) (boolToMaybe . f)) $ bs  

allM :: (Functor m, Monad m) => [a -> m Bool] -> a -> m Bool
allM bs a = helper bs a id

anyM :: (Functor m, Monad m) => [a -> m Bool] -> a -> m Bool
anyM bs a = helper bs a not

andM :: (Functor m, Monad m) => (a -> m Bool) -> (a -> m Bool) -> a -> m Bool
andM m1 m2 = allM [m1, m2]

orM :: (Functor m, Monad m) => (a -> m Bool) -> (a -> m Bool) -> a -> m Bool
orM m1 m2 = anyM [m1, m2]

kleisify f = return . ($) f
