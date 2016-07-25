module Text.Shaun.Sweeper
  ( SweeperT
  , Sweeper
  , to
  , getTo
  , at
  , getAt
  , back
  , get
  , withSweeperT
  , withSweeper
  , path
  , getPath
  , peek
  , modify
  )
where

import Text.Shaun.Types
import Control.Monad.Identity

import Data.List.Split (splitOn)

import Data.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

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

instance MonadTrans SweeperT where
  lift m = SweeperT $ \sv -> do
    ret <- m
    return (ret, sv)

instance MonadIO m => MonadIO (SweeperT m) where
  liftIO = lift . liftIO

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

-- | Go to an @SObject@'s attribute
to :: (Monad m) => String -> SweeperT m ()
to s = SweeperT $ \sv0 -> let sv1 = getSwObject s sv0 in return ((), FromObject s sv0 sv1)

-- | Go at a @SList@'s index
at :: (Monad m) => Int -> SweeperT m ()
at i = SweeperT $ \sv0 -> let sv1 = swat i sv0 in return ((), FromList i sv0 sv1)
-- at i = SweeperT $ \sv0 -> return ((), sv0)

-- | Perform a @to@ action and returns the current @ShaunValue@
-- @getTo attr = to attr >> get@
getTo :: (Monad m) => String -> SweeperT m ShaunValue
getTo s = to s >> get

-- | Perform an @at@ action and returns the current @ShaunValue@
-- @getAt i = at i >> get@
getAt :: (Monad m) => Int -> SweeperT m ShaunValue
getAt i = at i >> get

-- | Walk through a path composed of the successive objects' attributes names or
-- list indices separated by @':'@. (@path "attr1:[2]:attr2"@)
-- The resulting path action is just a composition of @to@ and @at@.
path :: (Monad m) => String -> SweeperT m ()
path s = foldl (>>) (return ()) $ map act (splitOn ":" s)
  where
    act ('[':rest) = (at . read . init) rest
    act s = to s

-- | Perform an @path@ action and returns the current @ShaunValue@
-- @getPath p = path p >> get@
getPath :: (Monad m) => String -> SweeperT m ShaunValue
getPath p = path p >> get

-- | Moves backward in the tree
back :: (Monad m) => SweeperT m ()
back = SweeperT $ \sv0 -> case sv0 of
  (FromObject _ b _) -> return ((), b)
  (FromList _ b _) -> return ((), b)
  Root _ -> return ((), sv0)

-- | Returns the current @ShaunValue@ the sweeper is at.
get :: (Monad m) => SweeperT m ShaunValue
get = SweeperT $ \sv0 -> case sv0 of
  FromObject _ _ ret -> return (ret, sv0)
  FromList _ _ ret -> return (ret, sv0)
  Root ret -> return (ret, sv0)

-- | Changes the current @ShaunValue@ with a new one.
set :: (Monad m) => ShaunValue -> SweeperT m ()
set sv = SweeperT $ \sv0 -> case sv0 of
  FromObject i sp _ -> return ((), FromObject i sp sv)
  FromList i sp _ -> return ((), FromList i sp sv)
  Root _ -> return ((), Root sv)

-- | Runs a @SweeperT@ with any monad @m@ and returns the computed value wrapped into @m@
withSweeperT :: (Monad m) => SweeperT m a -> ShaunValue -> m a
withSweeperT s v = fmap fst $ runSweeperT s (Root v)

-- | Runs a @Sweeper@ and returns a computed value
withSweeper :: Sweeper a -> ShaunValue -> a
withSweeper s v = runIdentity $ withSweeperT s v

-- | Performs a @Sweeper@ action without changing the position in the tree
peek :: (Monad m) => SweeperT m ShaunValue -> SweeperT m ShaunValue
peek sw = SweeperT $ \sv -> do
  (ret, _) <- runSweeperT sw sv
  return (ret, sv)

-- | Apply a function to the current @ShaunValue@
modify :: (Monad m) => (ShaunValue -> ShaunValue) -> SweeperT m ()
modify f = get >>= set . f
