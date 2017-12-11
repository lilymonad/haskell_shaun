{-# LANGUAGE
    DeriveAnyClass
  , UndecidableInstances
  , FlexibleInstances
  , MultiParamTypeClasses #-}

module Text.Shaun.Sweeper
  ( SweeperT
  , Sweeper
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
  , top
  , peek
  , modify
  )
where

import Text.Shaun.Types
import Control.Monad.Identity

import Data.List.Split (splitOn)

import Data.Maybe
import Data.Map as Map hiding (lookup, foldl, map)
import Data.Array as Arr

import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Monad.Catch
import Control.Monad.Except

data SwCrumb = FromObject String ShaunValue | FromList Int ShaunValue deriving (Eq)

type SwPath = (ShaunValue, [SwCrumb])

data SweeperT m a = SweeperT { runSweeperT :: SwPath -> m (a, SwPath) }
type Sweeper = SweeperT (Either ShaunException)

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
getSwObject s (o, _) = getObject s o

getObject s (SObject o) = case lookup s o of
  Nothing -> throwM AttributeNotFound
  Just r -> return r
getObject s _ = throwM NotAnObject

swat :: (MonadThrow m) => Int -> SwPath -> m ShaunValue
swat i ((SList l), _) = if length l > i
                          then return (l !! i)
                          else throwM OutOfRange
swat _ _ = throwM NotAList

-- | Go to an @SObject@'s attribute
to :: (MonadThrow m) => String -> SweeperT m ()
to s = SweeperT $ \(sv0, crumb) -> do
  sv1 <- getSwObject s (sv0, crumb)
  return ((), (sv1, (FromObject s sv0):crumb))

-- | Go at a @SList@'s index
at :: (MonadThrow m) => Int -> SweeperT m ()
at i = SweeperT $ \(sv0, crumb) -> do
  sv1 <- swat i (sv0, crumb)
  return ((), (sv1, (FromList i sv0):crumb))

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
back = SweeperT $ \(sv0, crumb) -> case crumb of
  (FromObject s b : crumb') -> return ((), (setObjectAttribute s sv0 b, crumb'))
  (FromList i b : crumb') -> return ((), (setListIndex i sv0 b, crumb'))
  _ -> return ((), (sv0, []))

setObjectAttribute str val (SObject m) = SObject $ Map.toList $ Map.insert str val $ Map.fromList m
setListIndex id val (SList l) = SList $ Arr.elems ((Arr.listArray (0, length l) l) Arr.// [(id,val)]) 
-- | Returns the current @ShaunValue@ the sweeper is at.
get :: (Monad m) => SweeperT m ShaunValue
get = SweeperT $ \(sv0, crumb) -> return (sv0, (sv0, crumb))

-- | Changes the current @ShaunValue@ with a new one.
set :: (Monad m) => ShaunValue -> SweeperT m ()
set sv = SweeperT $ \(sv0, crumb) -> return ((), (sv, crumb))

-- | Runs a @SweeperT@ with any monad @m@ and returns the computed value wrapped into @m@
withSweeperT :: (Monad m) => SweeperT m a -> ShaunValue -> m a
withSweeperT s v = fmap fst $ runSweeperT s (v, [])

-- | Runs a @Sweeper@ and returns a computed value
withSweeper :: Sweeper a -> ShaunValue -> Either ShaunException a
withSweeper s v = withSweeperT s v

-- | Performs a @Sweeper@ action without changing the position in the tree
peek :: (Monad m) => SweeperT m a -> SweeperT m a
peek sw = SweeperT $ \(sv, crumb) -> do
  (ret, (sv', [])) <- runSweeperT (do a <- sw; top; return a) (sv, [])
  return (ret, (sv', crumb))

-- | Gets to the root @ShaunValue@
top :: (Monad m) => SweeperT m ()
top = do
  cr <- getCrumbs
  case cr of
    [] -> return ()
    _  -> back >> top

getCrumbs :: (Monad m) => SweeperT m [SwCrumb]
getCrumbs = SweeperT $ \(a, b) -> return (b, (a,b))

-- | Apply a function to the current @ShaunValue@
modify :: (Monad m) => (ShaunValue -> ShaunValue) -> SweeperT m ()
modify f = get >>= set . f
