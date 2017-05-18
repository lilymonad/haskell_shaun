{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

module Text.Shaun.Types
  ( ShaunValue(..)
  , Shaun(..)
  , sLength
  , insert
  , remove
  , append
  )
where

-- | @ShaunValue@ represents a Haskell encoded SHAUN value.
data ShaunValue
  = SObject [(String, ShaunValue)]
  | SList [ShaunValue]
  | SNumber Double (Maybe String)
  | SString String
  | SBool Bool
  deriving (Eq)

-- | @Shaun a@ typeclass defines functions to convert type @a@ to and from
-- @ShaunValue@
class Shaun a where
  toShaun   :: a -> ShaunValue
  fromShaun :: ShaunValue -> a

instance Shaun ShaunValue where
  toShaun = id
  fromShaun = id

instance {-# OVERLAPS #-} (Integral a) => Shaun a where
  toShaun n = SNumber (fromIntegral n) Nothing
  fromShaun (SNumber n _) = truncate n

instance Shaun String where
  toShaun = SString
  fromShaun (SString s) = s
  fromShaun _ = ""

instance Shaun Double where
  toShaun n = SNumber n Nothing
  fromShaun (SNumber n _) = n
  fromShaun _ = 0

instance Shaun Float where
  toShaun n = SNumber (fromRational $ toRational n) Nothing
  fromShaun (SNumber n _) = fromRational $ toRational n

instance Shaun Bool where
  toShaun = SBool
  fromShaun (SBool b) = b

instance {-# OVERLAPS #-} Shaun a => Shaun [a] where
  toShaun = SList . map toShaun
  fromShaun (SList l) = map fromShaun l
  fromShaun _ = []

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


