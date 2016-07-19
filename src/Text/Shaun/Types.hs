{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Text.Shaun.Types
  ( ShaunValue(..)
  , Shaun(..)
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

instance (Integral a) => Shaun a where
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

instance Shaun a => Shaun [a] where
  toShaun = SList . map toShaun
  fromShaun (SList l) = map fromShaun l
  fromShaun _ = []
