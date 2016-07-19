{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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
  toShaun :: a -> ShaunValue
  fromShaun :: ShaunValue -> a

instance Shaun String where
  toShaun = SString
  fromShaun (SString s) = s
  fromShaun _ = ""

instance Shaun Double where
  toShaun n = SNumber n Nothing
  fromShaun (SNumber n _) = n
  fromShaun _ = 0

instance Shaun Bool where
  toShaun = SBool
  fromShaun (SBool b) = b

instance Shaun a => Shaun [a] where
  toShaun = SList . map toShaun
  fromShaun (SList l) = map fromShaun l
  fromShaun _ = []
