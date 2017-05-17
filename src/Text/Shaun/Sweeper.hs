{-# LANGUAGE
    DeriveAnyClass
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Text.Shaun.Sweeper
  ( SweeperT
  , Sweeper
  , SwException(..)
  , to
  , getTo
  , at
  , getAt
  , back
  , get
  , set
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

import Control.Monad.Catch
import Control.Monad.Except

data SwException
  = OutOfRange
  | AttributeNotFound
  | NotAList
  | NotAnObject
  deriving (Show, Exception)

data SwPath
  = Root ShaunValue
  | FromObject String SwPath ShaunValue
  | FromList Int SwPath ShaunValue
  deriving (Eq)

data SweeperT m a = SweeperT { runSweeperT :: SwPath -> m (a, SwPath) }
type Sweeper = SweeperT (Either SwException)

instance (MonadThrow m) => MonadThrow (SweeperT m) where
  throwM = lift . throwM

instance (MonadCatch m) => MonadCatch (SweeperT m) where
  catch m h = SweeperT $ \sv -> (runSweeperT m sv) `catch` (\e -> runSweeperT (h e) sv)

instance MonadError e m => MonadError e (SweeperT m) where
  throwError = lift . throwError
  catchError m h = SweeperT $ \sv -> (runSweeperT m sv) `catchError` (\e -> runSweeperT (h e) sv)

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

getSwObject :: (MonadThrow m) => String -> SwPath -> m ShaunValue
getSwObject s (FromObject _ _ o) = getObject s o
getSwObject s (FromList _ _ o) = getObject s o
getSwObject s (Root o) = getObject s o

getObject s (SObject o) = case lookup s o of
  Nothing -> throwM AttributeNotFound
  Just r -> return r
getObject s _ = throwM NotAnObject

swat :: (MonadThrow m) => Int -> SwPath -> m ShaunValue
swat i (FromObject _ _ (SList l)) = if length l > i
                          then return (l !! i)
                          else throwM OutOfRange

swat i (FromList _ _ (SList l)) = if length l > i
                          then return (l !! i)
                          else throwM OutOfRange

swat i (Root (SList l)) = if length l > i
                          then return (l !! i)
                          else throwM OutOfRange

swat _ _ = throwM NotAList

-- | Go to an @SObject@'s attribute
to :: (MonadThrow m) => String -> SweeperT m ()
to s = SweeperT $ \sv0 -> do
  sv1 <- getSwObject s sv0
  return ((), FromObject s sv0 sv1)

-- | Go at a @SList@'s index
at :: (MonadThrow m) => Int -> SweeperT m ()
at i = SweeperT $ \sv0 -> do
  sv1 <- swat i sv0
  return ((), FromList i sv0 sv1)

-- | Perform a @to@ action and returns the current @ShaunValue@
-- @getTo attr = to attr >> get@
getTo :: (MonadThrow m) => String -> SweeperT m ShaunValue
getTo s = to s >> get

-- | Perform an @at@ action and returns the current @ShaunValue@
-- @getAt i = at i >> get@
getAt :: (MonadThrow m) => Int -> SweeperT m ShaunValue
getAt i = at i >> get

-- | Walk through a path composed of the successive objects' attributes names or
-- list indices separated by @':'@. (@path "attr1:[2]:attr2"@)
-- The resulting path action is just a composition of @to@ and @at@.
path :: (MonadThrow m) => String -> SweeperT m ()
path s = foldl (>>) (return ()) $ map act (splitOn ":" s)
  where
    act ('[':rest) = (at . read . init) rest
    act s = to s

-- | Perform an @path@ action and returns the current @ShaunValue@
-- @getPath p = path p >> get@
getPath :: (MonadThrow m) => String -> SweeperT m ShaunValue
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
withSweeper :: Sweeper a -> ShaunValue -> Either SwException a
withSweeper s v = withSweeperT s v

-- | Performs a @Sweeper@ action without changing the position in the tree
peek :: (Monad m) => SweeperT m a -> SweeperT m a
peek sw = SweeperT $ \sv -> do
  (ret, _) <- runSweeperT sw sv
  return (ret, sv)

-- | Apply a function to the current @ShaunValue@
modify :: (Monad m) => (ShaunValue -> ShaunValue) -> SweeperT m ()
modify f = get >>= set . f
