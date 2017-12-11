{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Text.Shaun.Types
  ( ShaunValue(..)
  , Shaun(..)
  , sLength
  , insert
  , remove
  , append
  , ShaunException(..)
  )
where

import Control.Monad.Catch
-- import Control.Monad.Except
import Control.Monad

-- | @ShaunException@ represents an exception while processing a @ShaunValue@.
data ShaunException
  = OutOfRange
  | AttributeNotFound
  | NotAList
  | NotAnObject
  | NotAString
  | NotANumber
  | NotABool
  | Custom String
  deriving (Show)

instance Exception ShaunException

-- | @ShaunValue@ represents a Haskell encoded SHAUN value.
data ShaunValue
  = SObject [(String, ShaunValue)]
  | SList [ShaunValue]
  | SNumber Double (Maybe String)
  | SString String
  | SBool Bool
  | SNull
  deriving (Eq)

-- | @Shaun a@ typeclass defines functions to convert type @a@ to and from
-- @ShaunValue@
class Shaun a where
  toShaun   :: a -> ShaunValue
  fromShaun :: (MonadThrow m) => ShaunValue -> m a

instance Shaun ShaunValue where
  toShaun = id
  fromShaun = return

instance {-# OVERLAPS #-} (Integral a) => Shaun a where
  toShaun n = SNumber (fromIntegral n) Nothing
  fromShaun (SNumber n _) = return $ truncate n
  fromShaun _ = throwM NotANumber

instance Shaun String where
  toShaun = SString
  fromShaun (SString s) = return s
  fromShaun _ = throwM NotAString

instance Shaun Double where
  toShaun n = SNumber n Nothing
  fromShaun (SNumber n _) = return n
  fromShaun _ = throwM NotANumber

instance Shaun Float where
  toShaun n = SNumber (fromRational $ toRational n) Nothing
  fromShaun (SNumber n _) = return $ fromRational $ toRational n
  fromShaun _ = throwM NotANumber

instance Shaun Bool where
  toShaun = SBool
  fromShaun (SBool b) = return b
  fromShaun _ = throwM NotABool

instance {-# OVERLAPS #-} Shaun a => Shaun [a] where
  toShaun = SList . map toShaun
  fromShaun (SList l) = mapM fromShaun l
  fromShaun _ = throwM NotAList

-- | Gives an @SList@ or @SObject@ length, and returns @-1@ for other
-- constructors
sLength :: ShaunValue -> Int
sLength (SList l) = length l
sLength (SObject l) = length l
sLength _ = -1

-- | Inserts an attribute into an @SObject@
insert :: (String, ShaunValue) -> ShaunValue -> ShaunValue
insert p (SObject l) = SObject (p:l)
insert _ s = s

-- | Removes an attribute from an @SObject@
remove :: String -> ShaunValue -> ShaunValue
remove n (SObject l) = SObject $ remove' l n
  where
    remove' ((n,v):xs) n'
      | n == n' = xs
      | otherwise = (n,v) : remove' xs n
remove _ s = s

-- | Adds a value to a @SList@
append :: ShaunValue -> ShaunValue -> ShaunValue
append v (SList l) = SList (v:l)
append _ s = s


