module Text.Shaun.Sweeper
  ( SweeperT
  , Sweeper
  , goto
  , getTo
  , at
  , getAt
  , back
  , get
  , withSweeperT
  , withSweeper
  , path
  , getPath
  )
where

import Text.Shaun.Types
import Control.Monad.Identity

import Data.List.Split (splitOn)

import Data.Maybe

data SwPath
  = Root ShaunValue
  | FromObject String SwPath ShaunValue
  | FromList Int SwPath ShaunValue
  deriving (Eq)

data SweeperT m a = SweeperT { runSweeperT :: SwPath -> m (a, SwPath) }
type Sweeper = SweeperT Identity

instance Functor m => Functor (SweeperT m) where
  fmap f (SweeperT { runSweeperT = s }) =
    SweeperT (\sv0 -> fmap (\(a,sv) -> (f a,sv)) $ s sv0)

instance (Functor m, Monad m) => Applicative (SweeperT m) where
  pure a = SweeperT (\sv0 -> return (a, sv0))
  (SweeperT s0) <*> (SweeperT s1) = SweeperT $ \sv0 -> do
    { (f, sv1) <- s0 sv0
    ; (x, sv2) <- s1 sv1
    ; return (f x, sv2) }


instance Monad m => Monad (SweeperT m) where
  return v = SweeperT (\sv -> return (v, sv))

  (>>=) (SweeperT f) g = SweeperT $ \sv0 -> do { (a, sv1) <- f sv0
                                               ; runSweeperT (g a) sv1}

getSwObject s (FromObject _ _ o) = getObject s o
getSwObject s (FromList _ _ o) = getObject s o
getSwObject s (Root o) = getObject s o

getObject s (SObject o) = case lookup s o of
  Nothing -> error $ s ++ " doesn't exist"
  Just r -> r
getObject s _ = error $ s ++ " is not an object"

swat :: Int -> SwPath -> ShaunValue
swat i (FromObject _ _ o) = fromShaun o !! i
swat i (FromList _ _ o) = fromShaun o !! i
swat i (Root o) = fromShaun o !! i

goto :: (Monad m) => String -> SweeperT m ()
goto s = SweeperT $ \sv0 -> let sv1 = getSwObject s sv0 in return ((), FromObject s sv0 sv1)

getTo :: (Monad m) => String -> SweeperT m ShaunValue
getTo s = goto s >> get

getAt :: (Monad m) => Int -> SweeperT m ShaunValue
getAt i = at i >> get

getPath :: (Monad m) => String -> SweeperT m ShaunValue
getPath p = path p >> get

at :: (Monad m) => Int -> SweeperT m ()
at i = SweeperT $ \sv0 -> let sv1 = swat i sv0 in return ((), FromList i sv0 sv1)
-- at i = SweeperT $ \sv0 -> return ((), sv0)

path :: (Monad m) => String -> SweeperT m ()
path s = foldl (>>) (return ()) $ map act (splitOn ":" s)
  where
    act ('[':rest) = (at . read . init) rest
    act s = goto s

back :: (Monad m) => SweeperT m ()
back = SweeperT $ \sv0 -> case sv0 of
  (FromObject _ b _) -> return ((), b)
  (FromList _ b _) -> return ((), b)
  Root _ -> return ((), sv0)

get :: (Monad m) => SweeperT m ShaunValue
get = SweeperT $ \sv0 -> case sv0 of
  (FromObject _ _ ret) -> return (ret, sv0)
  (FromList _ _ ret) -> return (ret, sv0)
  Root ret -> return (ret, sv0)

withSweeperT :: (Monad m) => SweeperT m a -> ShaunValue -> m a
withSweeperT s v = fmap fst $ runSweeperT s (Root v)

withSweeper :: Sweeper a -> ShaunValue -> a
withSweeper s v = runIdentity $ withSweeperT s v

